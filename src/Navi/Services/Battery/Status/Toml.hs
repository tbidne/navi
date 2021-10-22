-- | This module provides toml configuration for the battery status service.
module Navi.Services.Battery.Status.Toml
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
import Navi.Services.Battery.Types (BatteryType (..))
import Navi.Services.Battery.Types qualified as BTypes
import Toml (TomlCodec, (.=))
import Toml qualified

-- | TOML for the battery status service.
data BatteryStatusToml = MkBatteryStatusToml
  { -- | Determines how we should query the system for battery information.
    batteryType :: BatteryType,
    -- | Determines how we treat repeat alerts.
    repeatEvent :: Maybe RepeatEvtToml,
    -- | Determines how we handle errors.
    errorNote :: Maybe ErrorNoteToml,
    -- | The alert for this service.
    note :: BatteryStatusNoteToml
  }
  deriving (Generic, Show)

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
  deriving (Generic, Show)

-- | Codec for 'BatteryStatusToml'.
batteryStatusCodec :: TomlCodec BatteryStatusToml
batteryStatusCodec =
  MkBatteryStatusToml
    <$> BTypes.batteryTypeCodec .= batteryType
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
