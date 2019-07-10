{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception (bracket)
import           Control.Monad     (replicateM_)
import           Network.Socket    (getSocketName)
import           SysLogTypes
import           UdpClient

main :: IO ()
main = bracket
         (openLog "127.0.0.1" "1514" "testprog")
         closeLog
         withSysLogHandler

withSysLogHandler :: SysLogHandle -> IO ()
withSysLogHandler hn = do
            sn <- getSocketName $ slSocket hn
            putStrLn $ "Client started on: " ++ show sn
            replicateM_ 1000 $ sysLog hn (Fac User) Info "this is a test message"
