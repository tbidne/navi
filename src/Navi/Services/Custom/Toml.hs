{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides toml configuration for the custom service.
module Navi.Services.Custom.Toml
  ( CustomToml (..),
    TriggerNoteToml (..),
  )
where

import Data.List.NonEmpty qualified as NE
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Navi.Data.CommandResultParser (CommandResultParserToml, commandResultParserDecoder)
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
  { -- | The notification to send when triggered.
    note :: NaviNote,
    -- | The text that triggers an alert.
    trigger :: Text
  }
  deriving stock (Eq, Show)

instance
  (k ~ A_Lens, a ~ NaviNote, b ~ NaviNote) =>
  LabelOptic "note" k TriggerNoteToml TriggerNoteToml a b
  where
  labelOptic =
    lensVL
      $ \f (MkTriggerNoteToml a1 a2) ->
        fmap
          (\b -> MkTriggerNoteToml b a2)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Text, b ~ Text) =>
  LabelOptic "trigger" k TriggerNoteToml TriggerNoteToml a b
  where
  labelOptic =
    lensVL
      $ \f (MkTriggerNoteToml a1 a2) ->
        fmap
          (\b -> MkTriggerNoteToml a1 b)
          (f a2)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance DecodeTOML TriggerNoteToml where
  tomlDecoder = do
    note <- tomlDecoder
    trigger <- getField "trigger"
    pure
      $ MkTriggerNoteToml
        { note,
          trigger
        }

-- | TOML for the custom service.
data CustomToml = MkCustomToml
  { -- | The command to run.
    command :: Command,
    -- | Determines how we handle errors.
    errEventCfg :: Maybe ErrorNoteToml,
    -- | An optional name to be used with logging.
    name :: Maybe Text,
    -- | Custom parsing.
    parser :: Maybe CommandResultParserToml,
    -- | The poll interval.
    pollInterval :: Maybe PollInterval,
    -- | Determines how we treat repeat alerts.
    repeatEventCfg :: Maybe (MultiRepeatEventToml Text),
    -- | The alert triggers.
    triggerNotes :: NonEmpty TriggerNoteToml
  }
  deriving stock (Eq, Show)

instance
  (k ~ A_Lens, a ~ Command, b ~ Command) =>
  LabelOptic "command" k CustomToml CustomToml a b
  where
  labelOptic =
    lensVL
      $ \f (MkCustomToml a1 a2 a3 a4 a5 a6 a7) ->
        fmap
          (\b -> MkCustomToml b a2 a3 a4 a5 a6 a7)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Maybe ErrorNoteToml, b ~ Maybe ErrorNoteToml) =>
  LabelOptic "errEventCfg" k CustomToml CustomToml a b
  where
  labelOptic =
    lensVL
      $ \f (MkCustomToml a1 a2 a3 a4 a5 a6 a7) ->
        fmap
          (\b -> MkCustomToml a1 b a3 a4 a5 a6 a7)
          (f a2)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Maybe Text, b ~ Maybe Text) =>
  LabelOptic "name" k CustomToml CustomToml a b
  where
  labelOptic =
    lensVL
      $ \f (MkCustomToml a1 a2 a3 a4 a5 a6 a7) ->
        fmap
          (\b -> MkCustomToml a1 a2 b a4 a5 a6 a7)
          (f a3)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Maybe CommandResultParserToml, b ~ Maybe CommandResultParserToml) =>
  LabelOptic "parser" k CustomToml CustomToml a b
  where
  labelOptic =
    lensVL
      $ \f (MkCustomToml a1 a2 a3 a4 a5 a6 a7) ->
        fmap
          (\b -> MkCustomToml a1 a2 a3 b a5 a6 a7)
          (f a4)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Maybe PollInterval, b ~ Maybe PollInterval) =>
  LabelOptic "pollInterval" k CustomToml CustomToml a b
  where
  labelOptic =
    lensVL
      $ \f (MkCustomToml a1 a2 a3 a4 a5 a6 a7) ->
        fmap
          (\b -> MkCustomToml a1 a2 a3 a4 b a6 a7)
          (f a5)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Maybe (MultiRepeatEventToml Text), b ~ Maybe (MultiRepeatEventToml Text)) =>
  LabelOptic "repeatEventCfg" k CustomToml CustomToml a b
  where
  labelOptic =
    lensVL
      $ \f (MkCustomToml a1 a2 a3 a4 a5 a6 a7) ->
        fmap
          (\b -> MkCustomToml a1 a2 a3 a4 a5 b a7)
          (f a6)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ NonEmpty TriggerNoteToml, b ~ NonEmpty TriggerNoteToml) =>
  LabelOptic "triggerNotes" k CustomToml CustomToml a b
  where
  labelOptic =
    lensVL
      $ \f (MkCustomToml a1 a2 a3 a4 a5 a6 a7) ->
        fmap
          (\b -> MkCustomToml a1 a2 a3 a4 a5 a6 b)
          (f a7)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance DecodeTOML CustomToml where
  tomlDecoder = do
    command <- commandDecoder
    errEventCfg <- errorNoteOptDecoder
    name <- getFieldOpt "name"
    parser <- commandResultParserDecoder
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
      $ MkCustomToml
        { command,
          errEventCfg,
          name,
          parser,
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
