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
import Navi.Services.Battery.Common (appCodec)
import Navi.Utils qualified as U
import Numeric.Data.Interval qualified as Interval
import Pythia.Services.Battery
  ( BatteryApp (..),
    BatteryPercentage (..),
    RunApp (..),
  )
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
  deriving (Eq, Show)

makeFieldLabelsNoPrefix ''BatteryPercentageNoteToml

-- | TOML for the battery percentage service.
data BatteryPercentageToml = MkBatteryPercentageToml
  { -- | All alerts for this service.
    alerts :: NonEmpty BatteryPercentageNoteToml,
    -- | The poll interval.
    pollInterval :: Maybe Word16,
    -- | Determines how we treat repeat alerts.
    repeatEvent :: Maybe RepeatEvtToml,
    -- | Determines how we handle errors.
    errorNote :: Maybe ErrorNoteToml,
    -- | Determines how we should query the system for battery information.
    app :: RunApp BatteryApp
  }
  deriving (Eq, Show)

makeFieldLabelsNoPrefix ''BatteryPercentageToml

-- | Codec for 'BatteryPercentageToml'.
batteryPercentageCodec :: TomlCodec BatteryPercentageToml
batteryPercentageCodec =
  MkBatteryPercentageToml
    <$> Toml.nonEmpty batteryPercentageNoteTomlCodec "alert" .= alerts
    <*> Toml.dioptional U.word16Codec .= pollInterval
    <*> Toml.dioptional EventToml.repeatEvtCodec .= repeatEvent
    <*> Toml.dioptional EventToml.errorNoteCodec .= errorNote
    <*> appCodec .= app

batteryPercentageNoteTomlCodec :: TomlCodec BatteryPercentageNoteToml
batteryPercentageNoteTomlCodec =
  MkBatteryPercentageNoteToml
    <$> percentageCodec .= percentage
    <*> Toml.dioptional NaviNote.urgencyLevelCodec .= urgency
    <*> Toml.dioptional NaviNote.timeoutCodec .= mTimeout

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
