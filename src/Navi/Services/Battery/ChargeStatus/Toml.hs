{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides toml configuration for the battery status service.
module Navi.Services.Battery.ChargeStatus.Toml
  ( BatteryStatusToml (..),
    BatteryStatusNoteToml (..),
    batteryStatusCodec,
  )
where

import DBus.Notify (Icon)
import Navi.Data.NaviNote (Timeout)
import Navi.Data.NaviNote qualified as NaviNote
import Navi.Event.Toml (ErrorNoteToml, RepeatEvtToml)
import Navi.Event.Toml qualified as EToml
import Navi.Prelude
import Optics.TH qualified as O
import System.Info.Services.Battery.ChargeStatus (Program (..))
import Toml (TomlCodec, (.=))
import Toml qualified

-- | TOML for the battery status notification.
data BatteryStatusNoteToml = MkBatteryStatusNoteToml
  { -- | The timeout for this alert.
    mTimeout :: Maybe Timeout,
    -- | The image for charging.
    mChargingImage :: Maybe Icon,
    -- | The image for discharging.
    mDischargingImage :: Maybe Icon,
    -- | The image for full.
    mFullImage :: Maybe Icon
  }
  deriving (Show)

O.makeFieldLabelsNoPrefix ''BatteryStatusNoteToml

-- | TOML for the battery status service.
data BatteryStatusToml = MkBatteryStatusToml
  { -- | Determines how we should query the system for battery information.
    program :: Program,
    -- | Determines how we treat repeat alerts.
    repeatEvent :: Maybe RepeatEvtToml,
    -- | Determines how we handle errors.
    errorNote :: Maybe ErrorNoteToml,
    -- | The alert for this service.
    note :: BatteryStatusNoteToml
  }
  deriving (Show)

O.makeFieldLabelsNoPrefix ''BatteryStatusToml

-- | Codec for 'BatteryStatusToml'.
batteryStatusCodec :: TomlCodec BatteryStatusToml
batteryStatusCodec =
  MkBatteryStatusToml
    <$> programCodec .= program
    <*> Toml.dioptional EToml.repeatEvtCodec .= repeatEvent
    <*> Toml.dioptional EToml.errorNoteCodec .= errorNote
    <*> batteryStatusNoteCodec .= note

batteryStatusNoteCodec :: TomlCodec BatteryStatusNoteToml
batteryStatusNoteCodec =
  MkBatteryStatusNoteToml
    <$> Toml.dioptional NaviNote.timeoutCodec .= mTimeout
    <*> Toml.dioptional (NaviNote.appImageKeyCodec "charging") .= mChargingImage
    <*> Toml.dioptional (NaviNote.appImageKeyCodec "discharing") .= mDischargingImage
    <*> Toml.dioptional (NaviNote.appImageKeyCodec "full") .= mFullImage

programCodec :: TomlCodec Program
programCodec =
  Toml.textBy showBatteryType parseBatteryType "type"
    <|> pure UPower
  where
    showBatteryType UPower = "upower"
    showBatteryType (Custom t) = t
    parseBatteryType "upower" = Right UPower
    parseBatteryType t = Right $ Custom t
