module Navi.Config.Toml
  ( ConfigToml (..),
    configCodec,
  )
where

import Data.Text qualified as T
import Katip (Severity (..))
import Navi.Config.Types (LogLoc (..), Logging (..))
import Navi.Data.NonNegative (NonNegative)
import Navi.Data.NonNegative qualified as NonNegative
import Navi.Prelude
import Navi.Services.Battery.Level.Toml as BatteryLevelToml
import Navi.Services.Battery.Status.Toml as BatteryStatusToml
import Navi.Services.Custom.Multiple.Toml as MultipleToml
import Navi.Services.Custom.Single.Toml as SingleToml
import Toml (TomlCodec, (.=))
import Toml qualified

data ConfigToml = MkConfigToml
  { pollToml :: NonNegative,
    logToml :: Logging,
    singleToml :: [SingleToml],
    multipleToml :: [MultipleToml],
    batteryLevelToml :: Maybe BatteryLevelToml,
    batteryStatusToml :: Maybe BatteryStatusToml
  }
  deriving (Generic, Show)

configCodec :: TomlCodec ConfigToml
configCodec =
  MkConfigToml
    <$> NonNegative.nonNegativeCodec "poll-interval" .= pollToml
    <*> Toml.table logCodec "logging" .= logToml
    <*> Toml.list SingleToml.singleCodec "single" .= singleToml
    <*> Toml.list MultipleToml.multipleCodec "multiple" .= multipleToml
    <*> Toml.dioptional (Toml.table BatteryLevelToml.batteryLevelCodec "battery-level") .= batteryLevelToml
    <*> Toml.dioptional (Toml.table BatteryStatusToml.batteryStatusCodec "battery-status") .= batteryStatusToml

logCodec :: TomlCodec Logging
logCodec =
  MkLogging
    <$> Toml.dioptional severityCodec .= severity
    <*> Toml.dioptional locationCodec .= location

severityCodec :: TomlCodec Severity
severityCodec =
  Toml.textBy showSeverity parseSeverity "severity"
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
locationCodec = Toml.textBy showLoc parseLoc "location"
  where
    showLoc Stdout = T.pack "stdout"
    showLoc (File f) = T.pack f
    parseLoc "stdout" = Right Stdout
    parseLoc f = Right $ File $ T.unpack f
