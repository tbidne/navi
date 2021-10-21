{-# LANGUAGE DuplicateRecordFields #-}

module Navi.Event.Types
  ( Command (..),
    Event (..),
    RepeatEvent (..),
    ErrorNote (..),
    EventErr (..),
    AnyEvent (..),
  )
where

import Data.Text qualified as T
import Navi.Data.NaviNote (NaviNote)
import Navi.Prelude
import Optics.Generic (GField (..))
import Optics.Operators ((^.))

newtype Command = MkCommand {getCommand :: Text}
  deriving (Generic, Show)

-- | Determines if we are allowed to send off duplicate notifications
-- simultaneously. If we are not, then 'NoRepeats' holds the last trigger
-- so that we can detect duplicates.
data RepeatEvent ref a
  = NoRepeats (ref (Maybe a))
  | AllowRepeats
  deriving (Generic)

instance Show (RepeatEvent ref a) where
  show (NoRepeats _) = "NoRepeats <ref>"
  show AllowRepeats = "AllowRepeats"

-- | Determines if we should send notifications for errors and, if so, if we
-- allow repeats.
data ErrorNote ref
  = NoErrNote
  | AllowErrNote (RepeatEvent ref ())
  deriving (Generic, Show)

-- | 'Event' represents sending notifications.
data Event ref a = MkEvent
  { name :: Text,
    command :: Command,
    parser :: Text -> Either EventErr a,
    raiseAlert :: a -> Maybe NaviNote,
    repeatEvent :: RepeatEvent ref a,
    errorNote :: ErrorNote ref
  }
  deriving (Generic)

instance Show (Event ref a) where
  show event =
    "MkEvent {name = "
      <> T.unpack (event ^. gfield @"name")
      <> ", command = "
      <> show (event ^. gfield @"command")
      <> ", parser = <func>, raiseAlert = <func>, repeatEvent = "
      <> show (event ^. gfield @"repeatEvent")
      <> ", errorNote = "
      <> show (event ^. gfield @"errorNote")
      <> "}"

-- | Represents an error when querying an 'Event'.
data EventErr = MkEventErr
  { name :: Text,
    short :: Text,
    long :: Text
  }
  deriving (Generic, Show)

type AnyEvent :: (Type -> Type) -> Type
data AnyEvent ref where
  MkAnyEvent :: (Eq a, Show a) => Event ref a -> AnyEvent ref

deriving instance Show (AnyEvent ref)
