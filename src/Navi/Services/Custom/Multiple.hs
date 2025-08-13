-- | This module provides a service for multiple alerts.
module Navi.Services.Custom.Multiple
  ( MultipleToml,
    toEvent,
  )
where

import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Navi.Data.NaviNote
  ( CustomResult (CustomOut, CustomText),
    NaviNote,
    replaceOut,
  )
import Navi.Data.PollInterval (PollInterval (MkPollInterval))
import Navi.Event.Toml qualified as EventToml
import Navi.Event.Types
  ( AnyEvent (MkAnyEvent),
    ErrorNote,
    Event
      ( MkEvent,
        errorNote,
        name,
        pollInterval,
        raiseAlert,
        repeatEvent,
        serviceType
      ),
    RepeatEvent,
  )
import Navi.Prelude
import Navi.Services.Custom.Multiple.Toml
  ( MultipleToml,
    TriggerNoteToml (MkTriggerNoteToml),
  )
import Navi.Services.Types (ServiceType (Multiple))
import Pythia.Data.Command (Command)

-- | Transforms toml configuration data into an 'AnyEvent'.
toEvent :: (MonadIORef m) => MultipleToml -> m AnyEvent
toEvent toml = do
  repeatEvent <- EventToml.mMultiRepeatEventTomlToVal (toml ^. #repeatEventCfg)
  errorNote <- EventToml.mErrorNoteTomlToVal (toml ^. #errEventCfg)
  pure
    $ MkAnyEvent
    $ mkMultipleEvent
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
  RepeatEvent CustomResult ->
  ErrorNote ->
  Event CustomResult
mkMultipleEvent mname cmd noteList pollInterval repeatEvent errorNote =
  MkEvent
    { name,
      serviceType = Multiple cmd,
      pollInterval,
      raiseAlert = \case
        CustomText result -> Map.lookup result noteMap
        CustomOut (result, out) -> replaceOut out <$> Map.lookup result noteMap,
      repeatEvent,
      errorNote
    }
  where
    noteMap = Map.fromList $ NE.toList noteList
    name = fromMaybe "multiple" mname
