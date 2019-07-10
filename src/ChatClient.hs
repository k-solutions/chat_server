{-# LANGUAGE RecordWildCards #-}

module ChatClient
  ( ClientName
  , Msg (..)
  , Client (..)
  , Server (..)
  , mkServer
  , mkClient
  , sendMsg
  ) where

import           Control.Concurrent.STM
import           Data.ByteString        (ByteString (..))
import           Data.Map               (Map (..))
import qualified Data.Map               as Map
import           System.IO              (Handle (..))

--  ---- Data Types -----

type ClientName  = ByteString

data Msg = Notice ByteString
         | Tell ClientName ByteString
         | Broadcast ClientName ByteString
         | Command ByteString
         deriving (Show, Eq)
         --
--  ---- Server Data ----

data Server = Server
            { srvClients :: TVar (Map ClientName Client)
            , srvChan    :: TChan Msg
            }

-- ---- Server IO Helper ----

mkServer :: IO Server
mkServer = do
    clientMap   <- newTVarIO Map.empty
    newBrdChan  <- newBroadcastTChanIO
    return Server { srvClients = clientMap
                  , srvChan    = newBrdChan
                  }

-- ---- Client Data ----

data Client = Client
            { clientName    :: ClientName
            , clientHandle  :: Handle
            , clientKicked  :: TVar (Maybe ByteString)
            , clientChan    :: TChan Msg
            , clientBrdChan :: TChan Msg
            }

-- ---- Client API ----

mkClient ::  ClientName
          -> Handle
          -> TChan Msg
          -> STM Client
mkClient name handle brdChan = do
  v <- newTVar Nothing
  c <- newTChan
  b <- dupTChan brdChan
  return Client  { clientName     = name
                 , clientHandle   = handle
                 , clientKicked   = v
                 , clientChan     = c
                 , clientBrdChan  = b
                 }

sendMsg ::  Client
         -> Msg
         -> STM ()
sendMsg Client{..} = writeTChan clientChan
