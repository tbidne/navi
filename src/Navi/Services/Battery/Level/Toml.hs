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
import Toml (TomlCodec, (.=))
import Toml qualified

data BatteryLevelToml = MkBatteryLevelToml
  { alerts :: [BatteryLevelNoteToml],
    repeatEvent :: Maybe RepeatEvtToml,
    errorNote :: Maybe ErrorNoteToml,
    batteryType :: BatteryType
  }
  deriving (Generic, Show)

data BatteryLevelNoteToml = MkBatteryLevelNoteToml
  { level :: BatteryLevel,
    urgency :: Maybe UrgencyLevel,
    mIcon :: Maybe Icon,
    mTimeout :: Maybe Timeout
  }
  deriving (Generic, Show)

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
