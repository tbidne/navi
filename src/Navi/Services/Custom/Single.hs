-- | This module provides a service for a single alert.
module Navi.Services.Custom.Single
  ( SingleToml,
    toEvent,
  )
where

import Control.Monad (guard)
import Data.Text qualified as T
import Navi.Data.CommandResult (CommandResult)
import Navi.Data.CommandResultParser
  ( CommandResultParserToml (MkCommandResultParserToml),
    defaultParser,
  )
import Navi.Data.NaviNote (NaviNote, replaceOut)
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
      (toml ^. #parser)
  where
    pi = fromMaybe (MkPollInterval 30) (toml ^. #pollInterval)
{-# INLINEABLE toEvent #-}

mkSingleEvent ::
  Maybe Text ->
  Command ->
  PollInterval ->
  (Text, NaviNote) ->
  RepeatEvent CommandResult ->
  ErrorNote ->
  Maybe CommandResultParserToml ->
  Event CommandResult CommandResult
mkSingleEvent mname cmd pollInterval (triggerVal, note) repeatEvent errorNote mParser =
  MkEvent
    { name,
      pollInterval,
      serviceType = Single cmd parser,
      raiseAlert = \r -> do
        guard (r ^. #result == triggerVal)
        pure $ case r ^. #output of
          Nothing -> (r, note)
          Just output -> (r, replaceOut output note),
      repeatEvent,
      errorNote
    }
  where
    name = fromMaybe "single" mname

    parser = case mParser of
      Just (MkCommandResultParserToml p) -> p name
      Nothing -> defaultParser
