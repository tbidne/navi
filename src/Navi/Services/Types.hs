-- | This module provides the 'ServiceType' type.
module Navi.Services.Types
  ( ServiceType (..),
  )
where

import Navi.Prelude
import System.Info.Data (Command)
import System.Info.Services.Battery.ChargeStatus (ChargeStatus)
import System.Info.Services.Battery.ChargeStatus qualified as ChargeStatus
import System.Info.Services.Battery.State (BatteryState)
import System.Info.Services.Battery.State qualified as State
import System.Info.Services.Network.Connection (Connection)
import System.Info.Services.Network.Connection qualified as Connection

-- | 'ServiceType' describes all implemented services.
-- It provides several built-in services for querying and parsing
-- system information (from the @system-info@ package), and then
-- 'Single' and 'Multiple' for custom services. It is a GADT so
-- we can link each service with its result type.
data ServiceType result where
  BatteryState :: State.Program -> ServiceType BatteryState
  BatteryChargeStatus :: ChargeStatus.Program -> ServiceType ChargeStatus
  NetworkConnection :: Connection.Program -> ServiceType Connection
  Single :: Command -> ServiceType Text
  Multiple :: Command -> ServiceType Text

deriving instance Show (ServiceType result)

deriving instance Eq (ServiceType result)
