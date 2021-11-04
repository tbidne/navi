{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides toml configuration for the battery level service.
module Navi.Services.Battery.State.Toml
  ( BatteryLevelToml (..),
    BatteryLevelNoteToml (..),
    batteryLevelCodec,
  )
where

import Control.Category ((>>>))
import DBus.Notify (Icon, UrgencyLevel)
import GHC.TypeNats (KnownNat)
import Navi.Data.NaviNote (Timeout)
import Navi.Data.NaviNote qualified as NaviNote
import Navi.Event.Toml (ErrorNoteToml, RepeatEvtToml)
import Navi.Event.Toml qualified as EventToml
import Navi.Prelude
import Optics.TH qualified as O
import Smart.Data.Math.BoundedNat (BoundedNat (..), mkBoundedNat)
import System.Info.Services.Battery.State (BatteryLevel, Program (..))
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

-- | TOML for each individual battery level.
data BatteryLevelNoteToml = MkBatteryLevelNoteToml
  { -- | The level for this alert.
    level :: BatteryLevel,
    -- | The urgency for this alert.
    urgency :: Maybe UrgencyLevel,
    -- | The image for this alert.
    mIcon :: Maybe Icon,
    -- | The timeout for this alert.
    mTimeout :: Maybe Timeout
  }
  deriving (Show)

O.makeFieldLabelsNoPrefix ''BatteryLevelNoteToml

-- | TOML for the battery level service.
data BatteryLevelToml = MkBatteryLevelToml
  { -- | All alerts for this service.
    alerts :: [BatteryLevelNoteToml],
    -- | Determines how we treat repeat alerts.
    repeatEvent :: Maybe RepeatEvtToml,
    -- | Determines how we handle errors.
    errorNote :: Maybe ErrorNoteToml,
    -- | Determines how we should query the system for battery information.
    program :: Program
  }
  deriving (Show)

O.makeFieldLabelsNoPrefix ''BatteryLevelToml

-- | Codec for 'BatteryLevelToml'.
batteryLevelCodec :: TomlCodec BatteryLevelToml
batteryLevelCodec =
  MkBatteryLevelToml
    <$> Toml.list batteryLevelNoteTomlCodec "alert" .= alerts
    <*> Toml.dioptional EventToml.repeatEvtCodec .= repeatEvent
    <*> Toml.dioptional EventToml.errorNoteCodec .= errorNote
    <*> programCodec .= program

batteryLevelNoteTomlCodec :: TomlCodec BatteryLevelNoteToml
batteryLevelNoteTomlCodec =
  MkBatteryLevelNoteToml
    <$> levelCodec .= level
    <*> Toml.dioptional NaviNote.urgencyLevelCodec .= urgency
    <*> Toml.dioptional NaviNote.appImageCodec .= mIcon
    <*> Toml.dioptional NaviNote.timeoutCodec .= mTimeout

programCodec :: TomlCodec Program
programCodec =
  Toml.textBy showBatteryType parseBatteryType "type"
    <|> pure UPower
  where
    showBatteryType UPower = "upower"
    showBatteryType (Custom t) = t
    parseBatteryType "upower" = Right UPower
    parseBatteryType t = Right $ Custom t

levelCodec :: TomlCodec BatteryLevel
levelCodec = boundedNCodec "level"

boundedNCodec :: (KnownNat l, KnownNat u) => Key -> TomlCodec (BoundedNat l u)
boundedNCodec = Toml.match _BoundedN

_BoundedN :: (KnownNat l, KnownNat u) => TomlBiMap (BoundedNat l u) AnyValue
_BoundedN = _BoundedNNatural >>> Toml._Natural

_BoundedNNatural :: (KnownNat l, KnownNat u) => TomlBiMap (BoundedNat l u) Natural
_BoundedNNatural = BiMap (Right . unBoundedNat) parseBounded
  where
    parseBounded =
      mkBoundedNat >.> \case
        Nothing -> Left $ ArbitraryError "Passed integer outside of bounds"
        Just n -> Right n