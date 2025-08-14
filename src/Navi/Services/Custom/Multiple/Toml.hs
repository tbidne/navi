{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides toml configuration for the custom multiple service.
module Navi.Services.Custom.Multiple.Toml
  ( MultipleToml (..),
    TriggerNoteToml (..),
  )
where

import Data.List.NonEmpty qualified as NE
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Navi.Data.NaviNote (NaviNote)
import Navi.Data.PollInterval (PollInterval, pollIntervalOptDecoder)
import Navi.Event.Toml
  ( ErrorNoteToml,
    MultiRepeatEventToml (MultiSomeRepeatsToml),
    errorNoteOptDecoder,
    multiRepeatEventOptDecoder,
  )
import Navi.Prelude
import Navi.Utils (commandDecoder)
import Pythia.Data.Command (Command)

-- | TOML for alerts.
data TriggerNoteToml = MkTriggerNoteToml
  { -- | The text that triggers an alert.
    trigger :: Text,
    -- | The notification to send when triggered.
    note :: NaviNote
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''TriggerNoteToml

-- | @since 0.1
instance DecodeTOML TriggerNoteToml where
  tomlDecoder =
    MkTriggerNoteToml
      <$> getField "trigger"
      <*> tomlDecoder

-- | TOML for the custom multiple service.
data MultipleToml = MkMultipleToml
  { -- | The command to run.
    command :: Command,
    -- | Determines how we handle errors.
    errEventCfg :: Maybe ErrorNoteToml,
    -- | An optional name to be used with logging.
    name :: Maybe Text,
    -- | The poll interval.
    pollInterval :: Maybe PollInterval,
    -- | Determines how we treat repeat alerts.
    repeatEventCfg :: Maybe (MultiRepeatEventToml Text),
    -- | The alert triggers.
    triggerNotes :: NonEmpty TriggerNoteToml
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''MultipleToml

-- | @since 0.1
instance DecodeTOML MultipleToml where
  tomlDecoder = do
    command <- commandDecoder
    errEventCfg <- errorNoteOptDecoder
    name <- getFieldOpt "name"
    pollInterval <- pollIntervalOptDecoder
    repeatEventCfg <- multiRepeatEventOptDecoder decodeStr
    triggerNotes <- getFieldWith tomlDecoder "trigger-note"

    case repeatEventCfg of
      Just (MultiSomeRepeatsToml txtRefs) -> do
        let triggers = mkTriggerTxts triggerNotes
            d = Set.difference txtRefs triggers
            msg =
              mconcat
                [ "Found repeat-events that referenced non-extant triggers. ",
                  "All references should correspond to a note 'trigger': ",
                  showSet d
                ]
        unless (Set.null d) $ fail msg
      _ -> pure ()

    pure
      $ MkMultipleToml
        { command,
          errEventCfg,
          name,
          pollInterval,
          repeatEventCfg,
          triggerNotes
        }
    where
      decodeStr (String s) = pure s
      decodeStr other = typeMismatch other

      mkTriggerTxts :: NonEmpty TriggerNoteToml -> Set Text
      mkTriggerTxts =
        Set.fromList
          . NE.toList
          . fmap (view #trigger)

      showSet :: Set Text -> String
      showSet =
        unpackText
          . (<> ".")
          . T.intercalate ", "
          . Set.toList
