module Navi.Services.Custom.Multiple
  ( MultipleToml,
    MultipleToml.multipleCodec,
    toMultipleEvent,
  )
where

import Navi.Event.Toml qualified as EventToml
import Navi.Event.Types (Event (..))
import Navi.MonadNavi (MonadNavi)
import Navi.Prelude
import Navi.Services.Custom.Multiple.Event qualified as MultipleEvent
import Navi.Services.Custom.Multiple.Toml (MultipleToml (..), TriggerNoteToml (..))
import Navi.Services.Custom.Multiple.Toml qualified as MultipleToml

toMultipleEvent :: MonadNavi m => MultipleToml -> m (Event m)
toMultipleEvent (MkMultipleToml cmd tn re ee) = do
  repeatEvt <- EventToml.mRepeatEvtTomlToVal re
  errorNote <- EventToml.mErrorNoteTomlToVal ee
  pure $ MultipleEvent.mkMultipleEvent cmd triggerNotes repeatEvt errorNote
  where
    triggerNotes = fmap toPair tn
    toPair (MkTriggerNoteToml t n) = (t, n)
