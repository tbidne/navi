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
import Navi.Services.Battery.Types (BatteryLevel)
import Toml (TomlCodec, (.=))
import Toml qualified

data BatteryToml = MkBatteryToml
  { alerts :: [BatteryLevelNoteToml],
    repeatEvent :: Maybe RepeatEvtToml,
    errorEvent :: Maybe ErrorNoteToml
  }
  deriving (Show)

batteryCodec :: TomlCodec BatteryToml
batteryCodec =
  MkBatteryToml
    <$> Toml.list batteryLevelNoteTomlCodec "alert" .= alerts
    <*> Toml.dioptional EventToml.repeatEvtCodec .= repeatEvent
    <*> Toml.dioptional EventToml.errorNoteCodec .= errorEvent

lvlCodec :: TomlCodec BatteryLevel
lvlCodec = BoundedN.boundedNCodec "level"

data BatteryLevelNoteToml = MkBatteryLevelNoteToml
  { level :: BatteryLevel,
    urgency :: UrgencyLevel,
    mIcon :: Maybe Icon,
    mTimeout :: Maybe Timeout
  }
  deriving (Show)

batteryLevelNoteTomlCodec :: TomlCodec BatteryLevelNoteToml
batteryLevelNoteTomlCodec =
  MkBatteryLevelNoteToml
    <$> lvlCodec .= level
    <*> NoteToml.urgencyLevelCodec .= urgency
    <*> NoteToml.appImageCodec .= mIcon
    <*> Toml.dioptional NoteToml.timeoutCodec .= mTimeout
