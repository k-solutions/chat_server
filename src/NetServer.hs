{-# LANGUAGE OverloadedStrings #-}

module NetServer
  ( talk
  , serverIO
  ) where

import           Control.Concurrent       (forkFinally)
import           Control.Concurrent.Async (race)
import           Control.Concurrent.STM
import qualified Control.Exception        as Ex (bracket)
import           Control.Monad            (forever, join, void)
import           Data.ByteString          (ByteString (..))
import qualified Data.ByteString.Char8    as BC
import           Data.Maybe               (fromMaybe)
import           Network.Socket
import           System.IO
import           Text.Printf

import           SocketIO

-- ---- Server API -----

serverIO :: IO ()
serverIO = do
    factor <- atomically $ newTVar 2
    sockIO port (connToTalk factor)
  where
    connToTalk factor conn = do
      handle <- socketToHandle conn ReadWriteMode
      forever $ talk handle factor

-- ---- IO Helpers ----

talk :: Handle -> TVar Integer -> IO ()
talk h factor = do
    hSetBuffering h LineBuffering
    c <- atomically newTChan
    race (server h factor c) (receive h c)
    return ()

server :: Handle -> TVar Integer -> TChan ByteString -> IO ()
server h factor c = do
    f <- readTVarIO factor
    hPrintf h "Current factor is: %d\n" f
    loop f
  where
    loop f =
      join . atomically $ do
        f' <- readTVar factor
        if f /= f'
          then  return (newFactor f')
          else  command f <$> readTChan c

    command f s
      | s == "q"         =
        BC.hPutStrLn h endMsg

      | BC.head s == '*' = do
        atomically $ writeTVar factor $ (bsToInteger . BC.tail) s
        loop f

      | otherwise = do
        BC.hPutStrLn h $ (toBs . (f *) . bsToInteger) s
        loop f

    newFactor newF = BC.hPutStrLn h (BC.concat ["New factor applied: ", (BC.pack . show) newF]) >> loop newF

receive :: Handle -> TChan ByteString -> IO ()
receive h c =   BC.hGetLine h
            >>= atomically . writeTChan c

-- ---- Helpers ----

bsToInteger :: ByteString -> Integer
bsToInteger = fst
            . fromMaybe (0, BC.empty)
            . BC.readInteger

toBs   = BC.pack
       . show

port   = "44444"
endMsg = "Thank you for using Haskell doubling service"
