{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides functionality for decoding data from a toml
-- configuration file.
module Navi.Config.Toml
  ( ConfigToml (..),
  )
where

import Data.Bytes (SomeSize)
import Data.Bytes qualified as Bytes
import Data.Char qualified as Ch
import Data.Text qualified as T
import FileSystem.OsPath (encodeFail)
import GHC.Real (truncate)
import Navi.Config.Phase (ConfigPhase (ConfigPhaseToml))
import Navi.Config.Types
  ( FilesSizeMode (FilesSizeModeDelete, FilesSizeModeWarn),
    LogLoc (DefPath, File, Stdout),
    Logging (MkLogging),
    NoteSystem (AppleScript, DBus, NotifySend),
  )
import Navi.Prelude
import Navi.Services.Battery.Percentage.Toml (BatteryPercentageToml)
import Navi.Services.Battery.Status.Toml (BatteryStatusToml)
import Navi.Services.Custom.Toml (CustomToml)
import Navi.Services.Network.NetInterfaces.Toml (NetInterfacesToml)
import Navi.Utils (getFieldOptArrayOf)

-- | 'ConfigToml' holds the data that is defined in the configuration file.
data ConfigToml = MkConfigToml
  { batteryPercentageToml :: Maybe BatteryPercentageToml,
    batteryStatusToml :: Maybe BatteryStatusToml,
    customToml :: [CustomToml],
    logToml :: Maybe Logging,
    netInterfacesToml :: [NetInterfacesToml],
    noteSystemToml :: Maybe (NoteSystem ConfigPhaseToml)
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''ConfigToml

-- | @since 0.1
instance DecodeTOML ConfigToml where
  tomlDecoder = do
    batteryPercentageToml <- getFieldOptWith tomlDecoder "battery-percentage"
    batteryStatusToml <- getFieldOptWith tomlDecoder "battery-status"
    customToml <- getFieldOptArrayOf "custom"
    logToml <- logDecoderOpt
    netInterfacesToml <- getFieldOptArrayOf "net-interface"
    noteSystemToml <- getFieldOptWith noteSystemDecoder "note-system"

    pure
      $ MkConfigToml
        { batteryPercentageToml,
          batteryStatusToml,
          customToml,
          logToml,
          netInterfacesToml,
          noteSystemToml
        }

logDecoderOpt :: Decoder (Maybe Logging)
logDecoderOpt = getFieldOptWith logDecoder "logging"

logDecoder :: Decoder Logging
logDecoder =
  MkLogging
    <$> severityDecoderOpt
    <*> locationDecoderOpt
    <*> sizeModeDecoderOpt

severityDecoderOpt :: Decoder (Maybe LogLevel)
severityDecoderOpt = setDef <$> getFieldOptWith severityDecoder "severity"
  where
    -- NOTE: We want the following semantics:
    --
    -- 1. User sets a log-level, use it.
    -- 2. User can disable logging w/ 'none'.
    -- 3. If no option is given, default to LevelError.
    --
    -- Hence 'getFieldOptWith severityDecoder' returns Maybe (Maybe LogLevel),
    -- where the outer maybe refers to the field existence, and the inner
    -- maybe handle 'none'.
    --
    -- Here we join the levels, so that Nothing means 'none', o/w logging is
    -- on.
    setDef Nothing = Just LevelError
    setDef (Just l) = l

severityDecoder :: Decoder (Maybe LogLevel)
severityDecoder =
  tomlDecoder >>= \case
    "debug" -> pure $ Just LevelDebug
    "info" -> pure $ Just LevelInfo
    "error" -> pure $ Just LevelError
    "none" -> pure Nothing
    bad -> fail $ unpackText $ "Unsupported severity: " <> bad

locationDecoderOpt :: Decoder (Maybe LogLoc)
locationDecoderOpt = getFieldOptWith locationDecoder "location"

locationDecoder :: Decoder LogLoc
locationDecoder =
  tomlDecoder >>= \case
    "default" -> pure DefPath
    "stdout" -> pure Stdout
    f -> File <$> encodeFail f

noteSystemDecoder :: Decoder (NoteSystem ConfigPhaseToml)
noteSystemDecoder =
  tomlDecoder >>= \case
    "apple-script" -> pure AppleScript
    "dbus" -> pure $ DBus ()
    "notify-send" -> pure NotifySend
    bad -> fail $ unpackText $ "Unsupported NoteSystem: " <> bad

sizeModeDecoderOpt :: Decoder (Maybe FilesSizeMode)
sizeModeDecoderOpt = getFieldOptWith sizeModeDecoder "size-mode"

sizeModeDecoder :: Decoder FilesSizeMode
sizeModeDecoder = do
  txt <- tomlDecoder
  let (m, byteTxt) = T.break Ch.isSpace txt
  cons <- case m of
    "warn" -> pure FilesSizeModeWarn
    "delete" -> pure FilesSizeModeDelete
    bad -> fail $ "Unrecognized size-mode: " <> unpackText bad
  case parseByteText byteTxt of
    Right b -> pure $ cons b
    Left err -> fail $ "Could not parse size-mode size: " <> unpackText err
  where
    parseByteText :: Text -> Either Text (Bytes B Natural)
    parseByteText txt =
      -- NOTE: Try conversion to natural first for more precision. Fall back
      -- to double if that fails.
      case Bytes.parse @(SomeSize Natural) txt of
        Right b -> Right $ Bytes.convert_ @_ @B b
        Left _ -> case Bytes.parse @(SomeSize Double) txt of
          Right b -> Right (truncate <$> Bytes.convert_ @_ @B b)
          Left err -> Left err
