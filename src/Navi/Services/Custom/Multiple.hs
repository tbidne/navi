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
  repeatEvt <- EventToml.mRepeatEvtTomlToVal (toml ^. #repeatEvtCfg)
  errorNote <- EventToml.mErrorNoteTomlToVal (toml ^. #errEvtCfg)
  pure $
    MkAnyEvent $
      mkMultipleEvent
        (toml ^. #command)
        triggerNotePairs
        pi
        repeatEvt
        errorNote
  where
    triggerNotePairs = fmap toPair (toml ^. #triggerNotes)
    toPair (MkTriggerNoteToml t n) = (t, n)
    pi = fromMaybe 30 (toml ^. #pollInterval)

mkMultipleEvent ::
  Command ->
  NonEmpty (Text, NaviNote) ->
  Word16 ->
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
