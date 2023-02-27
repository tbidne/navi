-- | This module provides a service for multiple alerts.
module Navi.Services.Custom.Multiple
  ( MultipleToml,
    toEvent,
  )
where

import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Navi.Data.NaviNote (NaviNote)
import Navi.Data.PollInterval (PollInterval (..))
import Navi.Event.Toml qualified as EventToml
import Navi.Event.Types
  ( AnyEvent (..),
    ErrorNote (..),
    Event (..),
    RepeatEvent (..),
  )
import Navi.Prelude
import Navi.Services.Custom.Multiple.Toml (MultipleToml, TriggerNoteToml (..))
import Navi.Services.Types (ServiceType (..))
import Pythia.Data.Command (Command)

-- | Transforms toml configuration data into an 'AnyEvent'.
toEvent :: (MonadIORef m) => MultipleToml -> m AnyEvent
toEvent toml = do
  repeatEvent <- EventToml.mRepeatEventTomlToVal (toml ^. #repeatEventCfg)
  errorNote <- EventToml.mErrorNoteTomlToVal (toml ^. #errEventCfg)
  pure $
    MkAnyEvent $
      mkMultipleEvent
        (toml ^. #name)
        (toml ^. #command)
        triggerNotePairs
        pi
        repeatEvent
        errorNote
  where
    triggerNotePairs = fmap toPair (toml ^. #triggerNotes)
    toPair (MkTriggerNoteToml t n) = (T.strip t, n)
    pi = fromMaybe (MkPollInterval 30) (toml ^. #pollInterval)
{-# INLINEABLE toEvent #-}

mkMultipleEvent ::
  Maybe Text ->
  Command ->
  NonEmpty (Text, NaviNote) ->
  PollInterval ->
  RepeatEvent Text ->
  ErrorNote ->
  Event Text
mkMultipleEvent mname cmd noteList pi re en =
  MkEvent
    { name = name',
      serviceType = Multiple cmd,
      pollInterval = pi,
      raiseAlert = flip Map.lookup noteMap,
      repeatEvent = re,
      errorNote = en
    }
  where
    noteMap = Map.fromList $ NE.toList noteList
    name' = fromMaybe "multiple" mname
