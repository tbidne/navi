{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides toml configuration for the battery level service.
module Navi.Services.Battery.Level.Toml
  ( BatteryLevelToml (..),
    BatteryLevelNoteToml (..),
    batteryLevelCodec,
  )
where

import DBus.Notify (Icon, UrgencyLevel)
import Navi.Data.NaviNote (Timeout)
import Navi.Data.NaviNote qualified as NaviNote
import Navi.Event.Toml (ErrorNoteToml, RepeatEvtToml)
import Navi.Event.Toml qualified as EventToml
import Navi.Prelude
import Navi.Services.Battery.Types (BatteryLevel, BatteryType (..))
import Navi.Services.Battery.Types qualified as BTypes
import Optics.TH qualified as O
import Toml (TomlCodec, (.=))
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
    batteryType :: BatteryType
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
    <*> BTypes.batteryTypeCodec .= batteryType

batteryLevelNoteTomlCodec :: TomlCodec BatteryLevelNoteToml
batteryLevelNoteTomlCodec =
  MkBatteryLevelNoteToml
    <$> BTypes.lvlCodec .= level
    <*> Toml.dioptional NaviNote.urgencyLevelCodec .= urgency
    <*> Toml.dioptional NaviNote.appImageCodec .= mIcon
    <*> Toml.dioptional NaviNote.timeoutCodec .= mTimeout
