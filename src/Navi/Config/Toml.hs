{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides functionality for decoding data from a toml
-- configuration file.
module Navi.Config.Toml
  ( ConfigToml (..),
  )
where

import Katip (Severity (..))
import Navi.Config.Types (LogLoc (..), Logging (..), NoteSystem (..))
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

severityDecoderOpt :: Decoder (Maybe Severity)
severityDecoderOpt = getFieldOptWith severityDecoder "severity"

severityDecoder :: Decoder Severity
severityDecoder =
  tomlDecoder >>= \case
    "debug" -> pure DebugS
    "InfoS" -> pure InfoS
    "ErrorS" -> pure ErrorS
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
