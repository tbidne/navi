module Navi.Services.Battery.Toml
  ( BatteryToml (..),
    BatteryLevelNoteToml (..),
    batteryCodec,
  )
where

import DBus.Notify (Icon, UrgencyLevel)
import Navi.Data.BoundedN qualified as BoundedN
import Navi.Data.NaviNote (Timeout)
import Navi.Data.NaviNote qualified as NaviNote
import Navi.Event.Toml (ErrorNoteToml, RepeatEvtToml)
import Navi.Event.Toml qualified as EventToml
import Navi.Prelude
import Navi.Services.Battery.Types (BatteryLevel, BatteryType (..))
import Toml (TomlCodec, (.=))
import Toml qualified

data BatteryToml = MkBatteryToml
  { alerts :: [BatteryLevelNoteToml],
    repeatEvent :: Maybe RepeatEvtToml,
    errorEvent :: Maybe ErrorNoteToml,
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

batteryCodec :: TomlCodec BatteryToml
batteryCodec =
  MkBatteryToml
    <$> Toml.list batteryLevelNoteTomlCodec "alert" .= alerts
    <*> Toml.dioptional EventToml.repeatEvtCodec .= repeatEvent
    <*> Toml.dioptional EventToml.errorNoteCodec .= errorEvent
    <*> batteryTypeCodec .= batteryType

lvlCodec :: TomlCodec BatteryLevel
lvlCodec = BoundedN.boundedNCodec "level"

batteryLevelNoteTomlCodec :: TomlCodec BatteryLevelNoteToml
batteryLevelNoteTomlCodec =
  MkBatteryLevelNoteToml
    <$> lvlCodec .= level
    <*> Toml.dioptional NaviNote.urgencyLevelCodec .= urgency
    <*> Toml.dioptional NaviNote.appImageCodec .= mIcon
    <*> Toml.dioptional NaviNote.timeoutCodec .= mTimeout

batteryTypeCodec :: TomlCodec BatteryType
batteryTypeCodec =
  Toml.textBy showBatteryType parseBatteryType "type"
    <|> pure UPower
  where
    showBatteryType UPower = "upower"
    showBatteryType (Custom t) = t
    parseBatteryType "upower" = Right UPower
    parseBatteryType t = Right $ Custom t
