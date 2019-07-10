{-# LANGUAGE OverloadedStrings #-}

module UdpClient where

import           Data.Bits
import           Data.ByteString           (ByteString (..))
import qualified Data.ByteString           as BS
import           Data.ByteString.Char8     (pack)
import           Network.Socket            hiding (sendTo)
import           Network.Socket.ByteString (sendTo)
import           SysLogTypes

data SysLogHandle = SysLogHandle
                  { slSocket  :: Socket
                  , slProgram :: ByteString
                  , slAddress :: SockAddr
                  }

-- ---- API -----

sysLog ::  SysLogHandle       -- ^ Socket info record
        -> Facility           -- ^ Reflect log message type
        -> Priority           -- ^ Priority of the message
        -> ByteString         -- ^ Log Message
        -> IO ()
sysLog slHandle fac priority msg = sendStr sendMsg
  where
    sendStr :: ByteString -> IO ()
    sendStr msg' | msg' == BS.empty = pure ()
                 | otherwise        = do
      sent <- sendTo (slSocket slHandle) msg'
                     (slAddress slHandle)
      sendStr $ BS.drop sent msg'

    sendMsg :: ByteString
    sendMsg  =  BS.concat ["<", code , ">"
                          , slProgram slHandle
                          , ": " , msg]

    code :: ByteString
    code  = pack . show
          $ makeCode fac priority

openLog ::  HostName          -- ^ Remote host
        ->  String            -- ^ Port number or name
        ->  String            -- ^ Name to log under
        ->  IO SysLogHandle   -- ^ Handle to use for logging
openLog hostName port logName = do
    let hints = defaultHints { addrSocketType = Datagram }
    addr:_  <- getAddrInfo (Just hints) (Just hostName) (Just port)
    sock    <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    let sockAddr = addrAddress addr
    connect sock sockAddr
    return $ SysLogHandle sock (pack logName) sockAddr

closeLog :: SysLogHandle -> IO ()
closeLog = close . slSocket

-- ---- Helpers ----

makeCode :: Facility -> Priority -> Int
makeCode fac priority =
    let facCode = fromEnum fac
        prtCode = fromEnum priority
    in (facCode `shiftL` 3) .|. prtCode
