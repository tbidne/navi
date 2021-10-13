module Navi.Event.Types
  ( Command (..),
    Event (..),
    RepeatEvent (..),
    ErrorNote (..),
    EventResult (..),
    EventErr (..),
  )
where

import DBus.Notify (Note)
import Navi.MonadNavi (Ref)
import Navi.Prelude

newtype Command = MkCommand {getCommand :: Text}
  deriving (Show)

-- | Determines if we are allowed to send off duplicate notifications
-- simultaneously. If we are not, then 'NoRepeats' holds the last trigger
-- so that we can detect duplicates.
data RepeatEvent m a
  = NoRepeats (Ref m (Maybe a))
  | AllowRepeats

-- | Determines if we should send notifications for errors and, if so, if we
-- allow repeats.
data ErrorNote m
  = NoErrNote
  | AllowErrNote (RepeatEvent m ())

-- | 'Event' represents sending notifications.
data Event m = MkEvent
  { trigger :: m EventResult,
    errorEvent :: ErrorNote m
  }

-- | The result from querying an 'Event'.
data EventResult
  = Err EventErr
  | None
  | Alert Note
  deriving (Show)

-- | Represents an error when querying an 'Event'.
data EventErr = MkEventErr
  { name :: Text,
    short :: Text,
    long :: Text
  }
  deriving (Show)
