module Navi.Services.Custom.Single
  ( SingleToml,
    SingleToml.singleCodec,
    toSingleEvent,
  )
where

import Navi.Event.Toml qualified as EventToml
import Navi.Event.Types (Event (..))
import Navi.Prelude
import Navi.Services.Custom.Single.Event qualified as SingleEvent
import Navi.Services.Custom.Single.Toml (SingleToml (..))
import Navi.Services.Custom.Single.Toml qualified as SingleToml

toSingleEvent :: SingleToml -> IO Event
toSingleEvent (MkSingleToml cmd tv n re ee) = do
  repeatEvt <- EventToml.mRepeatEvtTomlToVal re
  errorNote <- EventToml.mErrorNoteTomlToVal ee
  pure $ SingleEvent.mkSingleEvent cmd (tv, n) repeatEvt errorNote
