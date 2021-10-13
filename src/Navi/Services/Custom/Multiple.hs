module Navi.Services.Custom.Multiple
  ( MultipleToml,
    MultipleToml.multipleCodec,
    toMultipleEvent,
  )
where

import Navi.Event.Toml qualified as EventToml
import Navi.Event.Types (Event)
import Navi.MonadNavi (MonadNavi)
import Navi.Prelude
import Navi.Services.Custom.Multiple.Event qualified as MultipleEvent
import Navi.Services.Custom.Multiple.Toml (MultipleToml (..), TriggerNoteToml (..))
import Navi.Services.Custom.Multiple.Toml qualified as MultipleToml

toMultipleEvent :: MonadNavi m => MultipleToml -> m (Event m)
toMultipleEvent
  MkMultipleToml
    { command,
      triggerNotes,
      repeatEvtCfg,
      errEvtCfg
    } = do
    repeatEvt <- EventToml.mRepeatEvtTomlToVal repeatEvtCfg
    errorNote <- EventToml.mErrorNoteTomlToVal errEvtCfg
    pure $ MultipleEvent.mkMultipleEvent command triggerNotePairs repeatEvt errorNote
    where
      triggerNotePairs = fmap toPair triggerNotes
      toPair (MkTriggerNoteToml t n) = (t, n)
