-- | This module provides a service for a single alert.
module Navi.Services.Custom.Single
  ( SingleToml,
    toEvent,
  )
where

import Data.Text qualified as T
import Navi.Data.NaviNote (NaviNote)
import Navi.Data.PollInterval (PollInterval (..))
import Navi.Event.Toml qualified as EventToml
import Navi.Event.Types
  ( AnyEvent (..),
    ErrorNote (..),
    Event (..),
    RepeatEvent (..),
  )
import Navi.Prelude
import Navi.Services.Custom.Single.Toml (SingleToml)
import Navi.Services.Types (ServiceType (..))
import Pythia.Data.Command (Command)

-- | Transforms toml configuration data into an 'AnyEvent'.
toEvent :: (MonadIORef m) => SingleToml -> m AnyEvent
toEvent toml = do
  repeatEvent <- EventToml.mRepeatEventTomlToVal (toml ^. #repeatEventCfg)
  errorNote <- EventToml.mErrorNoteTomlToVal (toml ^. #errEventCfg)
  pure
    $ MkAnyEvent
    $ mkSingleEvent
      (toml ^. #name)
      (toml ^. #command)
      pi
      (T.strip (toml ^. #triggerVal), toml ^. #note)
      repeatEvent
      errorNote
  where
    pi = fromMaybe (MkPollInterval 30) (toml ^. #pollInterval)
{-# INLINEABLE toEvent #-}

mkSingleEvent ::
  Maybe Text ->
  Command ->
  PollInterval ->
  (Text, NaviNote) ->
  RepeatEvent Text ->
  ErrorNote ->
  Event Text
mkSingleEvent mname cmd pi (triggerVal, note) re en =
  MkEvent
    { name = name',
      pollInterval = pi,
      serviceType = Single cmd,
      raiseAlert = \b -> if b == triggerVal then Just note else Nothing,
      repeatEvent = re,
      errorNote = en
    }
  where
    name' = fromMaybe "single" mname
