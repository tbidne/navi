-- | This module provides the 'ServiceType' type.
module Navi.Services.Types
  ( ServiceType (..),
  )
where

import Navi.Prelude
import Pythia.Data (Command)
import Pythia.Services.Battery.ChargeStatus (BatteryChargeStatusApp, ChargeStatus)
import Pythia.Services.Battery.State (BatteryState, BatteryStateApp)
import Pythia.Services.Network.Connection (Connection, NetConnApp)

-- | 'ServiceType' describes all implemented services.
-- It provides several built-in services for querying and parsing
-- system information (from the @system-info@ package), and then
-- 'Single' and 'Multiple' for custom services. It is a GADT so
-- we can link each service with its result type.
data ServiceType result where
  BatteryState :: BatteryStateApp -> ServiceType BatteryState
  BatteryChargeStatus :: BatteryChargeStatusApp -> ServiceType ChargeStatus
  NetworkConnection :: NetConnApp -> ServiceType Connection
  Single :: Command -> ServiceType Text
  Multiple :: Command -> ServiceType Text

deriving instance Show (ServiceType result)

deriving instance Eq (ServiceType result)
