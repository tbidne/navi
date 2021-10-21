module Navi.Event.Types
  ( Command (..),
    Event (..),
    RepeatEvent (..),
    ErrorNote (..),
    EventErr (..),
    AnyEvent (..),
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
data Event ref a = MkEvent
  { eventName :: Text,
    command :: Command,
    parser :: Text -> Either EventErr a,
    raiseAlert :: a -> Maybe Note,
    repeatEvent :: RepeatEvent ref a,
    errorNote :: ErrorNote ref
  }

-- | Represents an error when querying an 'Event'.
data EventErr = MkEventErr
  { name :: Text,
    short :: Text,
    long :: Text
  }
  deriving (Show)

type AnyEvent :: (Type -> Type) -> Type
data AnyEvent ref where
  MkAnyEvent :: (Eq a, Show a) => Event ref a -> AnyEvent ref
