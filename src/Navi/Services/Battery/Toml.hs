module Navi.Services.Battery.Toml
  ( BatteryToml (..),
    BatteryLevelNoteToml (..),
    batteryCodec,
  )
where

import DBus.Notify (Icon, Timeout, UrgencyLevel)
import Navi.Data.BoundedN qualified as BoundedN
import Navi.Event.Toml (ErrorNoteToml, RepeatEvtToml)
import Navi.Event.Toml qualified as EventToml
import Navi.Note.Toml qualified as NoteToml
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
  deriving (Show)

data BatteryLevelNoteToml = MkBatteryLevelNoteToml
  { level :: BatteryLevel,
    urgency :: UrgencyLevel,
    mIcon :: Maybe Icon,
    mTimeout :: Maybe Timeout
  }
  deriving (Show)

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
    <*> NoteToml.urgencyLevelCodec .= urgency
    <*> NoteToml.appImageCodec .= mIcon
    <*> Toml.dioptional NoteToml.timeoutCodec .= mTimeout

batteryTypeCodec :: TomlCodec BatteryType
batteryTypeCodec =
  Toml.textBy showBatteryType parseBatteryType "type"
    <|> pure UPower
  where
    showBatteryType UPower = "upower"
    showBatteryType (Custom t) = t
    parseBatteryType "upower" = Right UPower
    parseBatteryType t = Right $ Custom t
