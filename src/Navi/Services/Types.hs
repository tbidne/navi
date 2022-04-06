-- | This module provides the 'ServiceType' type.
module Navi.Services.Types
  ( ServiceType (..),
  )
where

import Navi.Prelude
import Pythia.Data.Command (Command)
import Pythia.Services.Battery (Battery, BatteryConfig, BatteryStatus)
import Pythia.Services.NetInterface (NetInterface, NetInterfaceConfig)

-- | 'ServiceType' describes all implemented services.
-- It provides several built-in services for querying and parsing
-- system information (from the @pythia@ package), and then
-- 'Single' and 'Multiple' for custom services. It is a GADT so
-- we can link each service with its result type.
type ServiceType :: Type -> Type
data ServiceType result where
  BatteryPercentage :: BatteryConfig -> ServiceType Battery
  BatteryStatus :: BatteryConfig -> ServiceType BatteryStatus
  NetworkInterface :: NetInterfaceConfig -> ServiceType NetInterface
  Single :: Command -> ServiceType Text
  Multiple :: Command -> ServiceType Text

deriving instance Show (ServiceType result)

deriving instance Eq (ServiceType result)
