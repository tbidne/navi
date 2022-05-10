-- | This module provides a service for multiple alerts.
module Navi.Services.Custom.Multiple
  ( MultipleToml,
    MultipleToml.multipleCodec,
    toEvent,
  )
where

import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Navi.Data.NaviNote (NaviNote)
import Navi.Data.PollInterval (PollInterval (..))
import Navi.Effects (MonadMutRef)
import Navi.Event.Toml qualified as EventToml
import Navi.Event.Types
  ( AnyEvent (..),
    ErrorNote (..),
    Event (..),
    RepeatEvent (..),
  )
import Navi.Prelude
import Navi.Services.Custom.Multiple.Toml (MultipleToml, TriggerNoteToml (..))
import Navi.Services.Custom.Multiple.Toml qualified as MultipleToml
import Navi.Services.Types (ServiceType (..))
import Pythia.Data.Command (Command)

-- | Transforms toml configuration data into an 'AnyEvent'.
toEvent :: (MonadMutRef ref m) => MultipleToml -> m (AnyEvent ref)
toEvent toml = do
  repeatEvent <- EventToml.mRepeatEventTomlToVal (toml ^. #repeatEventCfg)
  errorNote <- EventToml.mErrorNoteTomlToVal (toml ^. #errEventCfg)
  pure $
    MkAnyEvent $
      mkMultipleEvent
        (toml ^. #command)
        triggerNotePairs
        pi
        repeatEvent
        errorNote
  where
    triggerNotePairs = fmap toPair (toml ^. #triggerNotes)
    toPair (MkTriggerNoteToml t n) = (t, n)
    pi = fromMaybe (MkPollInterval 30) (toml ^. #pollInterval)

mkMultipleEvent ::
  Command ->
  NonEmpty (Text, NaviNote) ->
  PollInterval ->
  RepeatEvent ref Text ->
  ErrorNote ref ->
  Event ref Text
mkMultipleEvent cmd noteList pi re en =
  MkEvent
    { name = "Multiple",
      serviceType = Multiple cmd,
      pollInterval = pi,
      raiseAlert = flip Map.lookup noteMap,
      repeatEvent = re,
      errorNote = en
    }
  where
    noteMap = Map.fromList $ NE.toList noteList
