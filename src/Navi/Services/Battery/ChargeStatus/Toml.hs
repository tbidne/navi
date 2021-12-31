{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides toml configuration for the battery status service.
module Navi.Services.Battery.ChargeStatus.Toml
  ( BatteryChargeStatusToml (..),
    BatteryChargeStatusNoteToml (..),
    batteryChargeStatusCodec,
  )
where

import Navi.Data.NaviNote (Timeout)
import Navi.Data.NaviNote qualified as NaviNote
import Navi.Event.Toml (ErrorNoteToml, RepeatEvtToml)
import Navi.Event.Toml qualified as EToml
import Navi.Prelude
import Optics.TH qualified as O
import Pythia.Services.Battery.ChargeStatus (BatteryChargeStatusApp (..))
import Toml (TomlCodec, (.=))
import Toml qualified

-- | TOML for the battery status notification.
newtype BatteryChargeStatusNoteToml = MkBatteryChargeStatusNoteToml
  { -- | The timeout for this alert.
    mTimeout :: Maybe Timeout
  }
  deriving (Show)

O.makeFieldLabelsNoPrefix ''BatteryChargeStatusNoteToml

-- | TOML for the battery status service.
data BatteryChargeStatusToml = MkBatteryChargeStatusToml
  { -- | Determines how we should query the system for battery information.
    program :: BatteryChargeStatusApp,
    -- | Determines how we treat repeat alerts.
    repeatEvent :: Maybe RepeatEvtToml,
    -- | Determines how we handle errors.
    errorNote :: Maybe ErrorNoteToml,
    -- | The alert for this service.
    note :: BatteryChargeStatusNoteToml
  }
  deriving (Show)

O.makeFieldLabelsNoPrefix ''BatteryChargeStatusToml

-- | Codec for 'BatteryChargeStatusToml'.
batteryChargeStatusCodec :: TomlCodec BatteryChargeStatusToml
batteryChargeStatusCodec =
  MkBatteryChargeStatusToml
    <$> programCodec .= program
    <*> Toml.dioptional EToml.repeatEvtCodec .= repeatEvent
    <*> Toml.dioptional EToml.errorNoteCodec .= errorNote
    <*> batteryStatusNoteCodec .= note

batteryStatusNoteCodec :: TomlCodec BatteryChargeStatusNoteToml
batteryStatusNoteCodec =
  MkBatteryChargeStatusNoteToml
    <$> Toml.dioptional NaviNote.timeoutCodec .= mTimeout

programCodec :: TomlCodec BatteryChargeStatusApp
programCodec =
  Toml.textBy showBatteryType parseBatteryType "type"
    <|> pure ChargeStatusUPower
  where
    showBatteryType ChargeStatusUPower = "upower"
    showBatteryType (ChargeStatusCustom t) = t
    parseBatteryType "upower" = Right ChargeStatusUPower
    parseBatteryType t = Right $ ChargeStatusCustom t
