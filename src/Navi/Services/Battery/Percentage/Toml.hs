{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module provides toml configuration for the battery percentage service.
module Navi.Services.Battery.Percentage.Toml
  ( BatteryPercentageToml (..),
    BatteryPercentageNoteToml (..),
    batteryPercentageCodec,
  )
where

import Control.Category ((>>>))
import DBus.Notify (UrgencyLevel)
import Navi.Data.NaviNote (Timeout)
import Navi.Data.NaviNote qualified as NaviNote
import Navi.Event.Toml (ErrorNoteToml, RepeatEvtToml)
import Navi.Event.Toml qualified as EventToml
import Navi.Prelude
import Numeric.Data.Interval qualified as Interval
import Pythia.Data.RunApp (RunApp (..))
import Pythia.Services.Battery (BatteryApp (..), BatteryConfig (..), BatteryPercentage (..))
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

-- | TOML for each individual battery percentage.
data BatteryPercentageNoteToml = MkBatteryPercentageNoteToml
  { -- | The percentage for this alert.
    percentage :: BatteryPercentage,
    -- | The urgency for this alert.
    urgency :: Maybe UrgencyLevel,
    -- | The timeout for this alert.
    mTimeout :: Maybe Timeout
  }
  deriving (Show)

makeFieldLabelsNoPrefix ''BatteryPercentageNoteToml

-- | TOML for the battery percentage service.
data BatteryPercentageToml = MkBatteryPercentageToml
  { -- | All alerts for this service.
    alerts :: [BatteryPercentageNoteToml],
    -- | Determines how we treat repeat alerts.
    repeatEvent :: Maybe RepeatEvtToml,
    -- | Determines how we handle errors.
    errorNote :: Maybe ErrorNoteToml,
    -- | Determines how we should query the system for battery information.
    program :: BatteryConfig
  }
  deriving (Show)

makeFieldLabelsNoPrefix ''BatteryPercentageToml

-- | Codec for 'BatteryPercentageToml'.
batteryPercentageCodec :: TomlCodec BatteryPercentageToml
batteryPercentageCodec =
  MkBatteryPercentageToml
    <$> Toml.list batteryPercentageNoteTomlCodec "alert" .= alerts
    <*> Toml.dioptional EventToml.repeatEvtCodec .= repeatEvent
    <*> Toml.dioptional EventToml.errorNoteCodec .= errorNote
    <*> programCodec .= program

batteryPercentageNoteTomlCodec :: TomlCodec BatteryPercentageNoteToml
batteryPercentageNoteTomlCodec =
  MkBatteryPercentageNoteToml
    <$> percentageCodec .= percentage
    <*> Toml.dioptional NaviNote.urgencyLevelCodec .= urgency
    <*> Toml.dioptional NaviNote.timeoutCodec .= mTimeout

programCodec :: TomlCodec BatteryConfig
programCodec =
  Toml.textBy showBatteryType parseBatteryType "type"
    <|> pure (MkBatteryConfig $ Single BatterySysFs)
  where
    showBatteryType (configToApp -> Single BatteryAcpi) = "acpi"
    showBatteryType (configToApp -> Single BatterySysFs) = "sysfs"
    showBatteryType (configToApp -> Single BatteryUPower) = "upower"
    showBatteryType _ = ""
    parseBatteryType "acpi" = Right (mkSingleApp BatteryAcpi)
    parseBatteryType "sysfs" = Right (mkSingleApp BatterySysFs)
    parseBatteryType "upower" = Right (mkSingleApp BatteryUPower)
    parseBatteryType t = Left t

percentageCodec :: TomlCodec BatteryPercentage
percentageCodec = boundedNCodec "percent"

boundedNCodec :: Key -> TomlCodec BatteryPercentage
boundedNCodec = Toml.match _BoundedN

_BoundedN :: TomlBiMap BatteryPercentage AnyValue
_BoundedN = _BoundedNNatural >>> Toml._Natural

_BoundedNNatural :: TomlBiMap BatteryPercentage Natural
_BoundedNNatural = BiMap (Right . fromIntegral . Interval.unLRInterval . unBatteryPercentage) parseBounded
  where
    parseBounded =
      (fmap MkBatteryPercentage . Interval.mkLRInterval . fromIntegral) >.> \case
        Nothing -> Left $ ArbitraryError "Passed integer outside of bounds"
        Just n -> Right n

mkSingleApp :: BatteryApp -> BatteryConfig
mkSingleApp = MkBatteryConfig . Single

configToApp :: BatteryConfig -> RunApp BatteryApp
configToApp (MkBatteryConfig app) = app