module Navi.Services.Battery.Types
  ( BatteryLevel,
    lvlCodec,
    BatteryStatus (..),
    BatteryState (..),
    BatteryType (..),
    batteryTypeCodec,
  )
where

import Navi.Data.BoundedN (BoundedN)
import Navi.Data.BoundedN qualified as BoundedN
import Navi.Prelude
import Toml (TomlCodec)
import Toml qualified

type BatteryLevel = BoundedN 0 100

lvlCodec :: TomlCodec BatteryLevel
lvlCodec = BoundedN.boundedNCodec "level"

data BatteryStatus
  = Charging
  | Discharging
  | Full
  deriving (Eq, Generic, Show)

data BatteryType
  = UPower
  | Custom Text
  deriving (Generic, Show)

batteryTypeCodec :: TomlCodec BatteryType
batteryTypeCodec =
  Toml.textBy showBatteryType parseBatteryType "type"
    <|> pure UPower
  where
    showBatteryType UPower = "upower"
    showBatteryType (Custom t) = t
    parseBatteryType "upower" = Right UPower
    parseBatteryType t = Right $ Custom t

data BatteryState = MkBatteryState
  { level :: BatteryLevel,
    status :: BatteryStatus
  }
  deriving (Eq, Generic, Show)
