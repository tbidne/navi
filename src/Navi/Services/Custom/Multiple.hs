-- | This module provides a service for multiple alerts.
module Navi.Services.Custom.Multiple
  ( MultipleToml,
    MultipleToml.multipleCodec,
    toEvent,
  )
where

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
import Navi.Services.Custom.Multiple.Toml (MultipleToml (..), TriggerNoteToml (..))
import Navi.Services.Custom.Multiple.Toml qualified as MultipleToml
import Navi.Services.Types (ServiceType (..))
import System.Info.Data (Command)

-- | Transforms toml configuration data into an 'AnyEvent'.
toEvent :: (MonadMutRef m ref) => MultipleToml -> m (AnyEvent ref)
toEvent
  MkMultipleToml
    { command,
      triggerNotes,
      repeatEvtCfg,
      errEvtCfg
    } = do
    repeatEvt <- EventToml.mRepeatEvtTomlToVal repeatEvtCfg
    errorNote <- EventToml.mErrorNoteTomlToVal errEvtCfg
    pure $ MkAnyEvent $ mkMultipleEvent command triggerNotePairs repeatEvt errorNote
    where
      triggerNotePairs = fmap toPair triggerNotes
      toPair (MkTriggerNoteToml t n) = (t, n)

mkMultipleEvent ::
  Command ->
  [(Text, NaviNote)] ->
  RepeatEvent ref Text ->
  ErrorNote ref ->
  Event ref Text
mkMultipleEvent cmd noteList re en =
  MkEvent
    { name = "Multiple",
      serviceType = Multiple cmd,
      raiseAlert = flip Map.lookup noteMap,
      repeatEvent = re,
      errorNote = en
    }
  where
    noteMap = Map.fromList noteList
