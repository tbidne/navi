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

data BatteryStatusToml = MkBatteryStatusToml
  { batteryType :: BatteryType,
    repeatEvent :: Maybe RepeatEvtToml,
    errorNote :: Maybe ErrorNoteToml,
    note :: BatteryStatusNoteToml
  }
  deriving (Generic, Show)

data BatteryStatusNoteToml = MkBatteryStatusNoteToml
  { mTimeout :: Maybe Timeout,
    mChargingImage :: Maybe Icon,
    mDischargingImage :: Maybe Icon,
    mFullImage :: Maybe Icon
  }
  deriving (Generic, Show)

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
