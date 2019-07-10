module UdpServer
  ( serverLog
  , plainHandler
  ) where

import           Data.ByteString           (ByteString (..))
import qualified Data.ByteString.Char8     as BC
import           Network.Socket            hiding (recvFrom)
import           Network.Socket.ByteString (recvFrom)

type FnHandler = SockAddr -> ByteString -> IO ()

serverLog ::  String                   -- ^ Port number or name : 514 is default
          ->  FnHandler                -- ^ Function to handle incoming messages
          ->  IO ()
serverLog port fnHand = withSocketsDo $ do
    addr:_ <- getAddrInfo
                  (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                  Nothing
                  (Just port)
    sock <- socket (addrFamily addr) Datagram defaultProtocol
    bind sock (addrAddress addr)
    procMessages sock
  where
    procMessages sock = do -- ^ get an UDP packet with max length 1024 bytes
      (msg, addr) <- recvFrom sock 1024
      fnHand addr msg
      procMessages sock

plainHandler :: FnHandler
plainHandler addr msg = putStrLn $ "From " ++ show addr ++ ": " ++ BC.unpack msg
