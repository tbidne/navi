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
import GHC.Real (truncate)
import Navi.Config.Types
  ( FilesSizeMode (..),
    LogLoc (..),
    Logging (..),
    NoteSystem (..),
  )
import Navi.Prelude
import Navi.Services.Battery.Percentage.Toml (BatteryPercentageToml)
import Navi.Services.Battery.Status.Toml (BatteryStatusToml)
import Navi.Services.Custom.Multiple.Toml (MultipleToml)
import Navi.Services.Custom.Single.Toml (SingleToml)
import Navi.Services.Network.NetInterfaces.Toml (NetInterfacesToml)
import Navi.Utils (getFieldOptArrayOf)

-- | 'ConfigToml' holds the data that is defined in the configuration file.
data ConfigToml = MkConfigToml
  { logToml :: !(Maybe Logging),
    noteSystemToml :: !(Maybe NoteSystem),
    singleToml :: ![SingleToml],
    multipleToml :: ![MultipleToml],
    batteryPercentageToml :: !(Maybe BatteryPercentageToml),
    batteryStatusToml :: !(Maybe BatteryStatusToml),
    netInterfacesToml :: ![NetInterfacesToml]
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''ConfigToml

-- | @since 0.1
instance DecodeTOML ConfigToml where
  tomlDecoder =
    MkConfigToml
      <$> logDecoderOpt
      <*> getFieldOptWith noteSystemDecoder "note-system"
      <*> getFieldOptArrayOf "single"
      <*> getFieldOptArrayOf "multiple"
      <*> getFieldOptWith tomlDecoder "battery-percentage"
      <*> getFieldOptWith tomlDecoder "battery-status"
      <*> getFieldOptArrayOf "net-interface"

logDecoderOpt :: Decoder (Maybe Logging)
logDecoderOpt = getFieldOptWith logDecoder "logging"

logDecoder :: Decoder Logging
logDecoder =
  MkLogging
    <$> severityDecoderOpt
    <*> locationDecoderOpt
    <*> sizeModeDecoderOpt

severityDecoderOpt :: Decoder (Maybe LogLevel)
severityDecoderOpt = getFieldOptWith severityDecoder "severity"

severityDecoder :: Decoder LogLevel
severityDecoder =
  tomlDecoder >>= \case
    "debug" -> pure LevelDebug
    "info" -> pure LevelInfo
    "error" -> pure LevelError
    bad -> fail $ unpack $ "Unsupported severity: " <> bad

locationDecoderOpt :: Decoder (Maybe LogLoc)
locationDecoderOpt = getFieldOptWith locationDecoder "location"

locationDecoder :: Decoder LogLoc
locationDecoder =
  tomlDecoder >>= \case
    "default" -> pure DefPath
    "stdout" -> pure Stdout
    f -> pure $ File $ unpack f

noteSystemDecoder :: Decoder NoteSystem
noteSystemDecoder =
  tomlDecoder >>= \case
    "dbus" -> pure DBus
    "notify-send" -> pure NotifySend
    bad -> fail $ unpack $ "Unsupported NoteSystem: " <> bad

sizeModeDecoderOpt :: Decoder (Maybe FilesSizeMode)
sizeModeDecoderOpt = getFieldOptWith sizeModeDecoder "size-mode"

sizeModeDecoder :: Decoder FilesSizeMode
sizeModeDecoder = do
  {-tomlDecoder >>= \case
    "default" -> pure DefPath
    "stdout" -> pure Stdout
    f -> pure $ File $ unpack f-}
  txt <- tomlDecoder
  let (m, byteTxt) = T.break Ch.isSpace txt
  cons <- case m of
    "warn" -> pure FileSizeModeWarn
    "delete" -> pure FileSizeModeDelete
    bad -> fail $ "Unrecognized file-log-size-mode: " <> unpack bad
  case parseByteText byteTxt of
    Right b -> pure $ cons b
    Left err -> fail $ "Could not parse --file-log-size-mode size: " <> unpack err
  where
    parseByteText :: Text -> Either Text (Bytes B Natural)
    parseByteText txt =
      -- NOTE: Try conversion to natural first for more precision. Fall back
      -- to double if that fails.
      case Bytes.parse @(SomeSize Natural) txt of
        Right b -> Right $ Bytes.convert (Proxy @B) b
        Left _ -> case Bytes.parse @(SomeSize Double) txt of
          Right b -> Right (truncate <$> Bytes.convert (Proxy @B) b)
          Left err -> Left err
