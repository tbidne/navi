-- | This module provides a service for multiple alerts.
module Navi.Services.Custom.Multiple
  ( MultipleToml,
    MultipleToml.multipleCodec,
    toMultipleEvent,
  )
where

import Navi.Effects (MonadMutRef)
import Navi.Event.Toml qualified as EventToml
import Navi.Event.Types (AnyEvent (..))
import Navi.Prelude
import Navi.Services.Custom.Multiple.Event qualified as MultipleEvent
import Navi.Services.Custom.Multiple.Toml (MultipleToml (..), TriggerNoteToml (..))
import Navi.Services.Custom.Multiple.Toml qualified as MultipleToml

-- | Transforms toml configuration data into an 'AnyEvent'.
toMultipleEvent :: (MonadMutRef m ref) => MultipleToml -> m (AnyEvent ref)
toMultipleEvent
  MkMultipleToml
    { command,
      triggerNotes,
      repeatEvtCfg,
      errEvtCfg
    } = do
    repeatEvt <- EventToml.mRepeatEvtTomlToVal repeatEvtCfg
    errorNote <- EventToml.mErrorNoteTomlToVal errEvtCfg
    pure $ MkAnyEvent $ MultipleEvent.mkMultipleEvent command triggerNotePairs repeatEvt errorNote
    where
      triggerNotePairs = fmap toPair triggerNotes
      toPair (MkTriggerNoteToml t n) = (t, n)
