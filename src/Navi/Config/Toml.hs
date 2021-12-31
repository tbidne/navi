{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides functionality for decoding data from a toml
-- configuration file.
module Navi.Config.Toml
  ( ConfigToml (..),
    configCodec,
  )
where

import Control.Category ((>>>))
import Data.Text qualified as T
import Katip (Severity (..))
import Navi.Config.Types (LogLoc (..), Logging (..))
import Navi.Prelude
import Navi.Services.Battery.ChargeStatus.Toml as BChargeStatusToml
import Navi.Services.Battery.State.Toml as BStateToml
import Navi.Services.Custom.Multiple.Toml as MultipleToml
import Navi.Services.Custom.Single.Toml as SingleToml
import Navi.Services.Network.Connectivity.Toml (NetworkConnectivityToml)
import Navi.Services.Network.Connectivity.Toml qualified as NetConnToml
import Optics.TH qualified as O
import Refined (NonNegative, Refined)
import Refined qualified as R
import Toml
  ( AnyValue,
    BiMap (..),
    Key,
    TomlBiMap,
    TomlBiMapError (..),
    TomlCodec,
    (.=),
  )
import Toml qualified

-- | 'ConfigToml' holds the data that is defined in the configuration file.
data ConfigToml = MkConfigToml
  { pollToml :: Refined NonNegative Int,
    logToml :: Logging,
    singleToml :: [SingleToml],
    multipleToml :: [MultipleToml],
    batteryStateToml :: Maybe BatteryStateToml,
    batteryChargeStatusToml :: Maybe BatteryChargeStatusToml,
    networkConnectivityToml :: [NetworkConnectivityToml]
  }
  deriving (Show)

-- | Toml decoder for 'ConfigToml'.
configCodec :: TomlCodec ConfigToml
configCodec =
  MkConfigToml
    <$> nonNegativeCodec "poll-interval" .= pollToml
    <*> Toml.table logCodec "logging" .= logToml
    <*> Toml.list SingleToml.singleCodec "single" .= singleToml
    <*> Toml.list MultipleToml.multipleCodec "multiple" .= multipleToml
    <*> Toml.dioptional (Toml.table BStateToml.batteryStateCodec "battery-state") .= batteryStateToml
    <*> Toml.dioptional (Toml.table BChargeStatusToml.batteryChargeStatusCodec "battery-charging") .= batteryChargeStatusToml
    <*> Toml.list NetConnToml.networkConnectivityCodec "network-connectivity" .= networkConnectivityToml

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

--- | Parses a TOML 'NonNegative'.
nonNegativeCodec :: Key -> TomlCodec (Refined NonNegative Int)
nonNegativeCodec = Toml.match _NonNegative

_NonNegative :: TomlBiMap (Refined NonNegative Int) AnyValue
_NonNegative = _NonNegativeInt >>> Toml._Int

_NonNegativeInt :: TomlBiMap (Refined NonNegative Int) Int
_NonNegativeInt = BiMap (Right . R.unrefine) parseNN
  where
    parseNN =
      R.refine >.> \case
        Left _ -> Left $ ArbitraryError "Passed negative to mkNonNegative"
        Right n -> Right n

O.makeFieldLabelsNoPrefix ''ConfigToml
