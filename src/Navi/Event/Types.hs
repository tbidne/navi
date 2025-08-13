{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides types for defining notification events.
module Navi.Event.Types
  ( Event (..),
    AnyEvent (..),
    RepeatEvent (..),
    ErrorNote (..),
    EventError (..),
    EventSuccess (..),
  )
where

import Data.Set (Set)
import GHC.Show (showSpace)
import Navi.Data.NaviNote (NaviNote)
import Navi.Data.PollInterval (PollInterval)
import Navi.Prelude
import Navi.Services.Types (ServiceType)
import Text.Show (Show (showsPrec), showParen, showString)

-- | Determines if we are allowed to send off duplicate notifications
-- simultaneously. If we are not, then 'NoRepeats' holds the last trigger
-- so that we can detect duplicates.
data RepeatEvent a
  = NoRepeats (IORef (Maybe a))
  | SomeRepeats (Set a) (IORef (Maybe a))
  | AllowRepeats

instance (Show a) => Show (RepeatEvent a) where
  showsPrec _ (NoRepeats _) = showString "NoRepeats <ref>"
  showsPrec i (SomeRepeats st _) =
    showParen
      (i >= 11)
      ( showString "SomeRepeats"
          . showSpace
          . showsPrec i st
          . showSpace
          . showString "<ref>"
      )
  showsPrec _ AllowRepeats = showString "AllowRepeats"

-- | Determines if we should send notifications for errors and, if so, if we
-- allow repeats.
data ErrorNote
  = NoErrNote
  | AllowErrNote (RepeatEvent ())
  deriving stock (Show)

-- | Represents an error when querying an 'Event'.
data EventError = MkEventError
  { -- | The name of the event.
    name :: Text,
    -- | Short description of the error.
    short :: Text,
    -- | Long description of the error.
    long :: Text
  }
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

makeFieldLabelsNoPrefix ''EventError

-- | 'Event' represents sending notifications. An event will:
--
-- 1. Query for information (i.e. run a shell command).
-- 2. Parse the result.
-- 3. Raise an alert if the result matches some condition.
data Event result = MkEvent
  { -- | Determines how we handle errors.
    errorNote :: ErrorNote,
    -- | The name of this event.
    name :: Text,
    -- | How often to poll for this event, in seconds.
    pollInterval :: PollInterval,
    -- | Conditionally raises an alert based on the result.
    raiseAlert :: result -> Maybe NaviNote,
    -- | Determines how we handle repeat alerts.
    repeatEvent :: RepeatEvent result,
    -- | The service to run.
    serviceType :: ServiceType result
  }

makeFieldLabelsNoPrefix ''Event

instance (Show result) => Show (Event result) where
  showsPrec i event =
    showParen
      (i >= 11)
      ( showString "MkEvent {errorNote = "
          . showsPrec i (event ^. #errorNote)
          . showString ", name = "
          . showsPrec i (event ^. #name)
          . showString ", pollInterval = "
          . showsPrec i (event ^. #pollInterval)
          . showString ", raiseAlert = <func>, repeatEvent = "
          . showsPrec i (event ^. #repeatEvent)
          . showString ", serviceType = "
          . showsPrec i (event ^. #serviceType)
          . showString "}"
      )

-- | Existentially quantifies result type on an 'Event'. Used so that we can
-- store different events in the same list.
type AnyEvent :: Type
data AnyEvent where
  MkAnyEvent :: (Ord result, Show result) => Event result -> AnyEvent

deriving stock instance Show AnyEvent

-- | Holds the 'Event' data used after an event is successfully run.
--
-- @since 0.1
data EventSuccess result = MkEventSuccess
  { result :: result,
    repeatEvent :: RepeatEvent result,
    raiseAlert :: result -> Maybe NaviNote
  }

makeFieldLabelsNoPrefix ''EventSuccess
