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
import Navi.Services.Battery.Percentage.Toml as BStateToml
import Navi.Services.Battery.Status.Toml as BChargeStatusToml
import Navi.Services.Custom.Multiple.Toml as MultipleToml
import Navi.Services.Custom.Single.Toml as SingleToml
import Navi.Services.Network.NetInterfaces.Toml (NetInterfacesToml)
import Navi.Services.Network.NetInterfaces.Toml qualified as NetConnToml
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
  { pollToml :: Word16,
    logToml :: Logging,
    singleToml :: [SingleToml],
    multipleToml :: [MultipleToml],
    batteryStateToml :: Maybe BatteryPercentageToml,
    batteryChargeStatusToml :: Maybe BatteryStatusToml,
    networkConnectivityToml :: [NetInterfacesToml]
  }
  deriving (Show)

-- | Toml decoder for 'ConfigToml'.
configCodec :: TomlCodec ConfigToml
configCodec =
  MkConfigToml
    <$> word16Codec "poll-interval" .= pollToml
    <*> Toml.table logCodec "logging" .= logToml
    <*> Toml.list SingleToml.singleCodec "single" .= singleToml
    <*> Toml.list MultipleToml.multipleCodec "multiple" .= multipleToml
    <*> Toml.dioptional (Toml.table BStateToml.batteryPercentageCodec "battery-percentage") .= batteryStateToml
    <*> Toml.dioptional (Toml.table BChargeStatusToml.batteryStatusCodec "battery-status") .= batteryChargeStatusToml
    <*> Toml.list NetConnToml.netInterfacesCodec "net-interface" .= networkConnectivityToml

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

--- | Parses a TOML 'Word16'.
word16Codec :: Key -> TomlCodec Word16
word16Codec = Toml.match _Word16

_Word16 :: TomlBiMap Word16 AnyValue
_Word16 = _Word16Int >>> Toml._Int

_Word16Int :: TomlBiMap Word16 Int
_Word16Int = BiMap (Right . fromIntegral) parseW16
  where
    parseW16 i
      | i < 0 = Left $ ArbitraryError $ "Received negative for word16: " <> showt i
      | i > w16ToInt (maxBound :: Word16) =
          Left $ ArbitraryError $ "Too large for word16: " <> showt i
      | otherwise = Right $ intToWord16 i
    intToWord16 :: Int -> Word16
    intToWord16 = fromIntegral

makeFieldLabelsNoPrefix ''ConfigToml
