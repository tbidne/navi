module Navi.Services.Battery.Types
  ( BatteryLevel,
    BatteryStatus (..),
    BatteryState (..),
    BatteryType (..),
  )
where

import Navi.Data.BoundedN (BoundedN)
import Navi.Prelude

type BatteryLevel = BoundedN 0 100

data BatteryStatus
  = Charging
  | Discharging
  | Full
  deriving (Eq, Show)

data BatteryType
  = UPower
  | Custom Text
  deriving (Show)

data BatteryState = MkBatteryState
  { level :: BatteryLevel,
    status :: BatteryStatus
  }
  deriving (Eq, Show)
