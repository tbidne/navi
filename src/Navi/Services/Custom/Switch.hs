-- | This module provides a service for a single alert.
module Navi.Services.Custom.Switch
  ( SwitchToml,
    toEvent,
  )
where

import Data.Text qualified as T
import Navi.Data.NaviNote (NaviNote)
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
    RepeatEvent,
  )
import Navi.Prelude
import Navi.Services.Custom.Switch.Toml (SwitchToml)
import Navi.Services.Types (ServiceType (Switch))
import Pythia.Data.Command (Command)

-- | Transforms toml configuration data into an 'AnyEvent'.
toEvent :: (MonadIORef m) => SwitchToml -> m AnyEvent
toEvent toml = do
  repeatEvent <- EventToml.mRepeatEventTomlToVal (toml ^. #repeatEventCfg)
  errorNote <- EventToml.mErrorNoteTomlToVal (toml ^. #errEventCfg)
  pure
    $ MkAnyEvent
    $ mkSwitchEvent
      (toml ^. #name)
      (toml ^. #command)
      pi
      (T.strip (toml ^. #triggerVal), toml ^. #note)
      repeatEvent
      errorNote
  where
    pi = fromMaybe (MkPollInterval 30) (toml ^. #pollInterval)
{-# INLINEABLE toEvent #-}

mkSwitchEvent ::
  Maybe Text ->
  Command ->
  PollInterval ->
  (Text, NaviNote) ->
  RepeatEvent Text ->
  ErrorNote ->
  Event Text
mkSwitchEvent mname cmd pi (triggerVal, note) re en =
  MkEvent
    { name = name',
      pollInterval = pi,
      serviceType = Switch cmd,
      raiseAlert,
      repeatEvent = re,
      errorNote = en
    }
  where
    name' = fromMaybe "switch" mname

    raiseAlert :: Text -> Maybe NaviNote
    raiseAlert result =
      let comp = result == triggerVal
          resultTxt = T.toLower $ showt comp
       in Just
            . set' #body (Just $ bodyPrefix <> ": " <> resultTxt)
            $ note

    bodyPrefix = fromMaybe "Status" (note ^. #body)
