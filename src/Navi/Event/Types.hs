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
import Navi.Prelude

newtype Command = MkCommand {getCommand :: Text}
  deriving (Show)

-- | Determines if we are allowed to send off duplicate notifications
-- simultaneously. If we are not, then 'NoRepeats' holds the last trigger
-- so that we can detect duplicates.
data RepeatEvent ref a
  = NoRepeats (ref (Maybe a))
  | AllowRepeats

-- | Determines if we should send notifications for errors and, if so, if we
-- allow repeats.
data ErrorNote ref
  = NoErrNote
  | AllowErrNote (RepeatEvent ref ())

-- | 'Event' represents sending notifications.
data Event m ref = MkEvent
  { trigger :: m EventResult,
    errorEvent :: ErrorNote ref
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
