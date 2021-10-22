-- | Provides types used by battery services.
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

-- | Represents battery levels in [0, 100].
type BatteryLevel = BoundedN 0 100

-- | Codec for 'BatteryLevel'.
lvlCodec :: TomlCodec BatteryLevel
lvlCodec = BoundedN.boundedNCodec "level"

-- | Represents battery statuses.
data BatteryStatus
  = Charging
  | Discharging
  | Full
  deriving (Eq, Generic, Show)

-- | Determines how we should query the system for battery information.
data BatteryType
  = UPower
  | Custom Text
  deriving (Generic, Show)

-- | Codec for 'BatteryType'.
batteryTypeCodec :: TomlCodec BatteryType
batteryTypeCodec =
  Toml.textBy showBatteryType parseBatteryType "type"
    <|> pure UPower
  where
    showBatteryType UPower = "upower"
    showBatteryType (Custom t) = t
    parseBatteryType "upower" = Right UPower
    parseBatteryType t = Right $ Custom t

-- | Full battery state, including level and status data.
data BatteryState = MkBatteryState
  { -- | The level data.
    level :: BatteryLevel,
    -- | The status data.
    status :: BatteryStatus
  }
  deriving (Eq, Generic, Show)
