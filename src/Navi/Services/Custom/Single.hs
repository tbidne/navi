-- | This module provides a service for a single alert.
module Navi.Services.Custom.Single
  ( SingleToml,
    toEvent,
  )
where

import Data.Text qualified as T
import Navi.Data.NaviNote
  ( CustomResult (CustomOut, CustomText),
    NaviNote,
    replaceOut,
  )
import Navi.Data.PollInterval (PollInterval (MkPollInterval))
import Navi.Event.Toml qualified as EventToml
import Navi.Event.Types
  ( AnyEvent (MkAnyEvent),
    ErrorNote,
    Event
      ( MkEvent,
        errorNote,
        name,
        pollInterval,
        raiseAlert,
        repeatEvent,
        serviceType
      ),
    RepeatEvent (..),
  )
import Navi.Prelude
import Navi.Services.Custom.Single.Toml (SingleToml)
import Navi.Services.Types (ServiceType (Single))
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
  RepeatEvent CustomResult ->
  ErrorNote ->
  Event CustomResult
mkSingleEvent mname cmd pollInterval (triggerVal, note) repeatEvent errorNote =
  MkEvent
    { name,
      pollInterval,
      serviceType = Single cmd,
      raiseAlert = \case
        CustomText result ->
          if result == triggerVal
            then Just note
            else Nothing
        CustomOut (result, out) ->
          if result == triggerVal
            then Just $ replaceOut out note
            else Nothing,
      repeatEvent,
      errorNote
    }
  where
    name = fromMaybe "single" mname
