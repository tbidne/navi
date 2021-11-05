-- | This module provides a service for a single alert.
module Navi.Services.Custom.Single
  ( SingleToml,
    SingleToml.singleCodec,
    toEvent,
  )
where

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
import Navi.Services.Custom.Single.Toml (SingleToml (..))
import Navi.Services.Custom.Single.Toml qualified as SingleToml
import Navi.Services.Types (ServiceType (..))
import System.Info.Data (Command)

-- | Transforms toml configuration data into an 'AnyEvent'.
toEvent :: (MonadMutRef m ref) => SingleToml -> m (AnyEvent ref)
toEvent
  MkSingleToml
    { command,
      triggerVal,
      note,
      repeatEvtCfg,
      errEvtCfg
    } = do
    repeatEvt <- EventToml.mRepeatEvtTomlToVal repeatEvtCfg
    errorNote <- EventToml.mErrorNoteTomlToVal errEvtCfg
    pure $ MkAnyEvent $ mkSingleEvent command (triggerVal, note) repeatEvt errorNote

mkSingleEvent ::
  Command ->
  (Text, NaviNote) ->
  RepeatEvent ref Text ->
  ErrorNote ref ->
  Event ref Text
mkSingleEvent cmd (triggerVal, note) re en =
  MkEvent
    { name = "Single",
      serviceType = Single cmd,
      raiseAlert = \b -> if b == triggerVal then Just note else Nothing,
      repeatEvent = re,
      errorNote = en
    }
