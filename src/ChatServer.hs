{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module ChatServer
  ( serverIO
  ) where

import           Control.Concurrent.Async (race)
import           Control.Concurrent.STM
import           Control.Exception        (finally, mask)
import           Control.Monad            (forever, join, when)
import           Data.ByteString          (ByteString (..))
import qualified Data.ByteString.Char8    as BC
import qualified Data.Map                 as Map
import           Network.Socket           (socketToHandle)
import           System.IO

import           ChatClient
import           SocketIO

-- ---- Server IO API ----

serverIO :: IO ()
serverIO = do
    newSrv <- mkServer
    sockIO port (talkTo newSrv)
  where
    talkTo server conn = do
      hdl    <- socketToHandle conn ReadWriteMode
      forever $ talk hdl server

talk ::  Handle
      -> Server
      -> IO ()
talk hdl server = do
    hSetNewlineMode hdl universalNewlineMode      -- ^ Swallow carriage returns sent
    hSetBuffering hdl LineBuffering
    readName
  where
    readName = do
      BC.hPutStrLn  hdl "Enter name: "
      name <- BC.hGetLine hdl
      if BC.null name
        then do
          BC.hPutStrLn hdl "Please enter non empty name: "
          readName
        else mask $ \restore -> do
          mbClient <- addClient server name hdl
          case mbClient of
            Nothing     -> do
              BC.hPutStrLn hdl $ BC.concat ["Username " , name, " is in use, try with other, please."]
              readName
            Just client ->
              restore (runClient server client)
                  `finally` removeClient server name

-- ---- Server API ----

broadcast ::  Server
           -> Msg
           -> STM ()
broadcast Server {..} = writeTChan srvChan

tell ::  Server
      -> ClientName
      -> Msg
      -> STM ()
tell Server{..} name msg = do
    cltMap <- readTVar srvClients
    case Map.lookup name cltMap of
      Nothing -> return ()
      Just c  -> sendMsg c msg

kick ::  Server
      -> ClientName
      -> ClientName
      -> STM ()
kick server@Server{..} by name = do
    cltMap <- readTVar srvClients
    case Map.lookup name cltMap of
      Nothing ->
        tell server by (Notice $ BC.concat [name, " is not connected"])
      Just c  -> do
        writeTVar (clientKicked c) $ Just $ BC.concat ["by ", by]
        tell server by (Notice $ BC.concat ["You kicked out: ", name])

-- ---- Client IO API ---

handleMsg ::  Server
           -> Client
           -> Msg
           -> IO Bool
handleMsg server Client{..} msg =
    case msg of
      Notice bsMsg         -> printBS $ BC.concat ["*** ", bsMsg]
      Tell from bsMsg      -> printBS $ BC.concat ["*", from, "* ", bsMsg]
      Broadcast from bsMsg -> printBS $ BC.concat ["<", from, "> ", bsMsg]
      Command bsCmd        -> handleCmd $ BC.words bsCmd
  where
    printBS bs = BC.hPutStrLn clientHandle bs >> return True

    handleCmd ::  [ByteString]
               -> IO Bool
    handleCmd  ["/quit"]            = return False
    handleCmd  ["/kick", who]       = do
                atomically $ kick server who clientName
                return True
    handleCmd  ("/tell":who:what)   = do
                atomically $ tell server who (Tell clientName $ BC.unwords what)
                return True
    handleCmd bsLst                 = broadcastOrBadCmd bsLst
      where
        broadcastOrBadCmd :: [ByteString] -> IO Bool
        broadcastOrBadCmd []    = return True --- printBS "Empty line!"
        broadcastOrBadCmd bs@(hd:_)
          | BC.head hd == '/' =
            printBS $ BC.concat ["Unrecognised command: ", hd]
          | otherwise                  = do
            atomically $ broadcast server (Broadcast clientName $ BC.unwords bs)
            return True

-- ---- Client IO Helpers ----

runClient ::  Server
           -> Client
           -> IO ()
runClient server@Server{..} client@Client{..} = do
    race serverTrd receiveTrd
    return ()
  where
    receiveTrd = forever $ do
      msg <- BC.hGetLine clientHandle
      atomically $ sendMsg client (Command msg)

    serverTrd = join $ atomically $ do
      k <- readTVar clientKicked
      case k of
        Just reason ->
          return $ BC.hPutStrLn clientHandle $ BC.concat ["You have been kicked: ", reason]
        Nothing     -> return $ do
          eitherMsg <- race (atomically $ readTChan clientChan) (atomically $ readTChan clientBrdChan)
          cont <- handleMsg server client (getAny eitherMsg)
          when cont serverTrd
    getAny (Left msg)  = msg
    getAny (Right msg) = msg

addClient ::  Server
           -> ClientName
           -> Handle
           -> IO (Maybe Client)
addClient server@Server{..} name hdl = atomically $ do
  clientMap <- readTVar srvClients
  if Map.member name clientMap
    then return Nothing
    else do
      client <- mkClient name hdl srvChan
      writeTVar srvClients $ Map.insert name client clientMap
      broadcast server $ conMsg $ BC.intercalate ", " (Map.keys clientMap)
      return $ Just client
  where conMsg allNames = connectMsg [name, " has connected. Welcome by: (", allNames, ")"]


removeClient ::  Server
              -> ClientName
              -> IO ()
removeClient server@Server{..} name = atomically $ do
  modifyTVar' srvClients $ Map.delete name
  broadcast server $ clientDisconnect name
  where clientDisconnect name  = connectMsg [name, " has disconnected"]

-- ---- Helpers ----

connectMsg =  Notice . BC.concat
port = "44444"
