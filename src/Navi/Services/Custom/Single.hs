module Navi.Services.Custom.Single
  ( SingleToml,
    SingleToml.singleCodec,
    toSingleEvent,
  )
where

import Navi.Event.Toml qualified as EventToml
import Navi.Event.Types (Event)
import Navi.MonadNavi (MonadNavi)
import Navi.Prelude
import Navi.Services.Custom.Single.Event qualified as SingleEvent
import Navi.Services.Custom.Single.Toml (SingleToml (..))
import Navi.Services.Custom.Single.Toml qualified as SingleToml

toSingleEvent :: MonadNavi m => SingleToml -> m (Event m)
toSingleEvent
  MkSingleToml
    { command,
      triggerVal,
      note,
      repeatEvtCfg,
      errEvtCfg
    } = do
    repeatEvt <- EventToml.mRepeatEvtTomlToVal repeatEvtCfg
    errorNote <- EventToml.mErrorNoteTomlToVal errEvtCfg
    pure $ SingleEvent.mkSingleEvent command (triggerVal, note) repeatEvt errorNote
