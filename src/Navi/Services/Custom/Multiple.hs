-- | This module provides a service for multiple alerts.
module Navi.Services.Custom.Multiple
  ( MultipleToml,
    toEvent,
  )
where

import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Navi.Data.CommandResult (CommandResult (MkCommandResult))
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
    RepeatEvent,
  )
import Navi.Prelude
import Navi.Services.Custom.Multiple.Toml
  ( MultipleToml,
    TriggerNoteToml (MkTriggerNoteToml),
  )
import Navi.Services.Types (ServiceType (Multiple))
import Pythia.Data.Command (Command)

-- | Transforms toml configuration data into an 'AnyEvent'.
toEvent :: (MonadIORef m) => MultipleToml -> m AnyEvent
toEvent toml = do
  repeatEvent <- EventToml.mMultiRepeatEventTomlToVal toCommandResult (toml ^. #repeatEventCfg)
  errorNote <- EventToml.mErrorNoteTomlToVal (toml ^. #errEventCfg)
  pure
    $ MkAnyEvent
    $ mkMultipleEvent
      (toml ^. #name)
      (toml ^. #command)
      triggerNotePairs
      pi
      repeatEvent
      errorNote
      (toml ^. #parser)
  where
    triggerNotePairs = fmap toPair (toml ^. #triggerNotes)
    toPair (MkTriggerNoteToml t n) = (T.strip t, n)
    pi = fromMaybe (MkPollInterval 30) (toml ^. #pollInterval)

    toCommandResult = MkCommandResult Nothing Nothing
{-# INLINEABLE toEvent #-}

mkMultipleEvent ::
  Maybe Text ->
  Command ->
  NonEmpty (Text, NaviNote) ->
  PollInterval ->
  RepeatEvent CommandResult ->
  ErrorNote ->
  Maybe CommandResultParserToml ->
  Event CommandResult CommandResult
mkMultipleEvent mname cmd noteList pollInterval repeatEvent errorNote mParser =
  MkEvent
    { name,
      serviceType = Multiple cmd parser,
      pollInterval,
      raiseAlert = \r -> do
        note <- Map.lookup (r ^. #result) noteMap

        pure $ case r ^. #output of
          Nothing -> (r, note)
          Just output -> (r, replaceOut output note),
      repeatEvent,
      errorNote
    }
  where
    noteMap = Map.fromList $ NE.toList noteList
    name = fromMaybe "multiple" mname

    parser = case mParser of
      Just (MkCommandResultParserToml p) -> p name
      Nothing -> defaultParser
