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
import Navi.Services.Custom.Single.Toml (SingleToml)
import Navi.Services.Custom.Single.Toml qualified as SingleToml
import Navi.Services.Types (ServiceType (..))
import Pythia.Data.Command (Command)

-- | Transforms toml configuration data into an 'AnyEvent'.
toEvent :: (MonadMutRef ref m) => SingleToml -> m (AnyEvent ref)
toEvent toml = do
  repeatEvt <- EventToml.mRepeatEvtTomlToVal (toml ^. #repeatEvtCfg)
  errorNote <- EventToml.mErrorNoteTomlToVal (toml ^. #errEvtCfg)
  pure $
    MkAnyEvent $
      mkSingleEvent
        (toml ^. #command)
        pi
        (toml ^. #triggerVal, toml ^. #note)
        repeatEvt
        errorNote
  where
    pi = fromMaybe 30 (toml ^. #pollInterval)

mkSingleEvent ::
  Command ->
  Word16 ->
  (Text, NaviNote) ->
  RepeatEvent ref Text ->
  ErrorNote ref ->
  Event ref Text
mkSingleEvent cmd pi (triggerVal, note) re en =
  MkEvent
    { name = "Single",
      pollInterval = pi,
      serviceType = Single cmd,
      raiseAlert = \b -> if b == triggerVal then Just note else Nothing,
      repeatEvent = re,
      errorNote = en
    }
