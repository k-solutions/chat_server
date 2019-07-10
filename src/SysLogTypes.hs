module SysLogTypes
  ( Priority (..)
  , Facility (..)
  , MainFacility (..)
  ) where

import           Data.Tuple (swap)

-- | Priorities and log messages types

data Priority = Debug
              | Info
              | Notice
              | Warning
              | Error
              | Critical
              | Alert
              | Emergency
              deriving (Eq, Ord, Show, Read, Enum)

-- | Facilities are where the messages are sent to
data MainFacility = Kern            -- ^ Kernel messages
                  | User            -- ^ Userland messages
                  | Mail            -- ^ Email system
                  | Daemon          -- ^ Daemon (long running processes) messages
                  | Auth            -- ^ Authentication and security messages
                  | Syslog          -- ^ Internal syslog messages
                  | LPR             -- ^ Printer messages
                  | News            -- ^ Usenet news
                  | UUCP            -- ^ UUCP messages
                  | Cron            -- ^ Cron messages
                  | AuthPriv        -- ^ Private authentication messages
                  | FTP             -- ^ FTP messages
                  deriving (Eq, Show, Read, Enum)

data Facility = Fac MainFacility
              | Local Int
              deriving (Show, Read, Eq)

instance Enum Facility where
    toEnum n | n >= locMinBound = Local (n - locMinBound)
             | n <= facMaxBound = Fac (toEnum n)
             | otherwise        = error $ "Bad Facility code detected in: " ++ show n
    fromEnum (Local n) = n + locMinBound
    fromEnum (Fac a)   = fromEnum a

facToCode :: [(Facility, Int)]
facToCode =   map toFacTpl (enumFrom Kern)
          ++  map toLocTpl allLocals
  where
    toFacTpl x = (Fac x, fromEnum x)
    toLocTpl x = (x, fromEnum x)

codeToFac :: [(Int, Facility)]
codeToFac = map swap facToCode

-- ---- Helpers ----
locMinBound = 16
facMaxBound = 12

allLocals :: [Facility]
allLocals = map Local [0..7]
