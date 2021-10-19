module Navi.Services.Custom.Single
  ( SingleToml,
    SingleToml.singleCodec,
    toSingleEvent,
  )
where

import Navi.Effects (MonadMutRef)
import Navi.Event.Toml qualified as EventToml
import Navi.Event.Types (AnyEvent (..))
import Navi.Prelude
import Navi.Services.Custom.Single.Event qualified as SingleEvent
import Navi.Services.Custom.Single.Toml (SingleToml (..))
import Navi.Services.Custom.Single.Toml qualified as SingleToml

toSingleEvent :: (MonadMutRef m ref) => SingleToml -> m (AnyEvent ref)
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
    pure $ MkAnyEvent $ SingleEvent.mkSingleEvent command (triggerVal, note) repeatEvt errorNote
