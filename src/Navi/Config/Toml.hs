{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides functionality for decoding data from a toml
-- configuration file.
module Navi.Config.Toml
  ( ConfigToml (..),
    configCodec,
  )
where

import Data.Text qualified as T
import Katip (Severity (..))
import Navi.Config.Types (LogLoc (..), Logging (..))
import Navi.Prelude
import Navi.Services.Battery.Percentage.Toml as BStateToml
import Navi.Services.Battery.Status.Toml as BChargeStatusToml
import Navi.Services.Custom.Multiple.Toml as MultipleToml
import Navi.Services.Custom.Single.Toml as SingleToml
import Navi.Services.Network.NetInterfaces.Toml (NetInterfacesToml)
import Navi.Services.Network.NetInterfaces.Toml qualified as NetConnToml
import Toml (TomlCodec, (.=))
import Toml qualified

-- | 'ConfigToml' holds the data that is defined in the configuration file.
data ConfigToml = MkConfigToml
  { logToml :: !(Maybe Logging),
    singleToml :: ![SingleToml],
    multipleToml :: ![MultipleToml],
    batteryPercentageToml :: !(Maybe BatteryPercentageToml),
    batteryStatusToml :: !(Maybe BatteryStatusToml),
    netInterfacesToml :: ![NetInterfacesToml]
  }
  deriving stock (Eq, Show)

-- | Toml decoder for 'ConfigToml'.
configCodec :: TomlCodec ConfigToml
configCodec =
  MkConfigToml
    <$> Toml.dioptional (Toml.table logCodec "logging") .= logToml
    <*> Toml.list SingleToml.singleCodec "single" .= singleToml
    <*> Toml.list MultipleToml.multipleCodec "multiple" .= multipleToml
    <*> Toml.dioptional (Toml.table BStateToml.batteryPercentageCodec "battery-percentage") .= batteryPercentageToml
    <*> Toml.dioptional (Toml.table BChargeStatusToml.batteryStatusCodec "battery-status") .= batteryStatusToml
    <*> Toml.list NetConnToml.netInterfacesCodec "net-interface" .= netInterfacesToml

logCodec :: TomlCodec Logging
logCodec =
  MkLogging
    <$> severityCodec .= severity
    <*> locationCodec .= location

severityCodec :: TomlCodec Severity
severityCodec =
  Toml.textBy showSeverity parseSeverity "severity"
    <|> pure ErrorS
  where
    showSeverity DebugS = "debug"
    showSeverity InfoS = "info"
    showSeverity ErrorS = "error"
    showSeverity _ = "unknown"
    parseSeverity "debug" = Right DebugS
    parseSeverity "info" = Right InfoS
    parseSeverity "error" = Right ErrorS
    parseSeverity other =
      Left $
        "Unsupported severity: "
          <> other
          <> ". Should be one of <info|error>."

locationCodec :: TomlCodec LogLoc
locationCodec =
  Toml.textBy showLoc parseLoc "location"
    <|> pure DefPath
  where
    showLoc DefPath = "default"
    showLoc Stdout = T.pack "stdout"
    showLoc (File f) = T.pack f
    parseLoc "stdout" = Right Stdout
    parseLoc "default" = Right DefPath
    parseLoc f = Right $ File $ T.unpack f

makeFieldLabelsNoPrefix ''ConfigToml
