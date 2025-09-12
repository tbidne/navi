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
import Navi.Event.Types.EventError (EventError (MkEventError, long, name, short))
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

-- | 'Event' represents sending notifications. An event will:
--
-- 1. Query for information (i.e. run a shell command).
-- 2. Parse the result.
-- 3. Map the result to a trigger.
-- 4. Raise an alert if the trigger matches some condition.
--
-- For most services, the (result -> trigger) map will be trivial i.e.
-- result and trigger will be identical.
--
-- For example, custom servicess is 'Text', and NetInterfaces is
-- NetInterface. In these cases, there is an exact
-- correspondonce between the service query result and the trigger.
--
-- But for e.g. battery percentage, the result is Battery (percentage and
-- status) whereas trigger is PercentageData.
--
-- See NOTE: [Battery Percentage Result/Trigger] for an explanation on why
-- this might be desirable.
data Event result trigger = MkEvent
  { -- | Determines how we handle errors.
    errorNote :: ErrorNote,
    -- | The name of this event.
    name :: Text,
    -- | How often to poll for this event, in seconds.
    pollInterval :: PollInterval,
    -- | Conditionally raises an alert based on the (result -> trigger)
    -- mapping.
    raiseAlert :: result -> Maybe (trigger, NaviNote),
    -- | Determines how we handle repeat alerts.
    repeatEvent :: RepeatEvent trigger,
    -- | The service to run.
    serviceType :: ServiceType result
  }

instance
  (k ~ A_Lens, a ~ ErrorNote, b ~ ErrorNote) =>
  LabelOptic "errorNote" k (Event result trigger) (Event result trigger) a b
  where
  labelOptic =
    lensVL
      $ \f (MkEvent a1 a2 a3 a4 a5 a6) ->
        fmap
          (\b -> MkEvent b a2 a3 a4 a5 a6)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Text, b ~ Text) =>
  LabelOptic "name" k (Event result trigger) (Event result trigger) a b
  where
  labelOptic =
    lensVL
      $ \f (MkEvent a1 a2 a3 a4 a5 a6) ->
        fmap
          (\b -> MkEvent a1 b a3 a4 a5 a6)
          (f a2)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ PollInterval, b ~ PollInterval) =>
  LabelOptic "pollInterval" k (Event result trigger) (Event result trigger) a b
  where
  labelOptic =
    lensVL
      $ \f (MkEvent a1 a2 a3 a4 a5 a6) ->
        fmap
          (\b -> MkEvent a1 a2 b a4 a5 a6)
          (f a3)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ (result -> Maybe (trigger, NaviNote)), b ~ (result -> Maybe (trigger, NaviNote))) =>
  LabelOptic "raiseAlert" k (Event result trigger) (Event result trigger) a b
  where
  labelOptic =
    lensVL
      $ \f (MkEvent a1 a2 a3 a4 a5 a6) ->
        fmap
          (\b -> MkEvent a1 a2 a3 b a5 a6)
          (f a4)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ RepeatEvent trigger, b ~ RepeatEvent trigger) =>
  LabelOptic "repeatEvent" k (Event result trigger) (Event result trigger) a b
  where
  labelOptic =
    lensVL
      $ \f (MkEvent a1 a2 a3 a4 a5 a6) ->
        fmap
          (\b -> MkEvent a1 a2 a3 a4 b a6)
          (f a5)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ ServiceType result, b ~ ServiceType result) =>
  LabelOptic "serviceType" k (Event result trigger) (Event result trigger) a b
  where
  labelOptic =
    lensVL
      $ \f (MkEvent a1 a2 a3 a4 a5 a6) ->
        fmap
          (\b -> MkEvent a1 a2 a3 a4 a5 b)
          (f a6)
  {-# INLINE labelOptic #-}

instance (Show trigger) => Show (Event result trigger) where
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
  MkAnyEvent :: (Ord trigger, Show result, Show trigger) => Event result trigger -> AnyEvent

deriving stock instance Show AnyEvent

-- | Holds the 'Event' data used after an event is successfully run.
--
-- @since 0.1
data EventSuccess result trigger = MkEventSuccess
  { pollInterval :: Maybe PollInterval,
    raiseAlert :: result -> Maybe (trigger, NaviNote),
    repeatEvent :: RepeatEvent trigger,
    result :: result
  }

instance
  (k ~ A_Lens, a ~ Maybe PollInterval, b ~ Maybe PollInterval) =>
  LabelOptic "pollInterval" k (EventSuccess result trigger) (EventSuccess result trigger) a b
  where
  labelOptic =
    lensVL
      $ \f (MkEventSuccess a1 a2 a3 a4) ->
        fmap
          (\b -> MkEventSuccess b a2 a3 a4)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ (result -> Maybe (trigger, NaviNote)), b ~ (result -> Maybe (trigger, NaviNote))) =>
  LabelOptic "raiseAlert" k (EventSuccess result trigger) (EventSuccess result trigger) a b
  where
  labelOptic =
    lensVL
      $ \f (MkEventSuccess a1 a2 a3 a4) ->
        fmap
          (\b -> MkEventSuccess a1 b a3 a4)
          (f a2)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ RepeatEvent trigger, b ~ RepeatEvent trigger) =>
  LabelOptic "repeatEvent" k (EventSuccess result trigger) (EventSuccess result trigger) a b
  where
  labelOptic =
    lensVL
      $ \f (MkEventSuccess a1 a2 a3 a4) ->
        fmap
          (\b -> MkEventSuccess a1 a2 b a4)
          (f a3)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ result, b ~ result) =>
  LabelOptic "result" k (EventSuccess result trigger) (EventSuccess result trigger) a b
  where
  labelOptic =
    lensVL
      $ \f (MkEventSuccess a1 a2 a3 a4) ->
        fmap
          (\b -> MkEventSuccess a1 a2 a3 b)
          (f a4)
  {-# INLINE labelOptic #-}
