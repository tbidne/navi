{-# LANGUAGE TemplateHaskell #-}

-- | This module provides the 'ServiceType' type.
module Navi.Services.Types
  ( ServiceType (..),
    _BatteryPercentage,
    _BatteryStatus,
    _Custom,
    _NetworkInterface,
  )
where

import Navi.Data.CommandResult (CommandResult)
import Navi.Data.CommandResultParser (CommandResultParser)
import Navi.Prelude
import Pythia.Data.Command (Command)
import Pythia.Services.Battery (Battery, BatteryApp, BatteryStatus)
import Pythia.Services.NetInterface (Device, NetInterface, NetInterfaceApp)

-- | 'ServiceType' describes all implemented services.
-- It provides several built-in services for querying and parsing
-- system information (from the @pythia@ package), and then
-- 'Single' and 'Custom' for custom services. It is a GADT so
-- we can link each service with its result type.
type ServiceType :: Type -> Type
data ServiceType result where
  BatteryPercentage :: BatteryApp -> ServiceType Battery
  BatteryStatus :: BatteryApp -> ServiceType BatteryStatus
  NetworkInterface :: Device -> NetInterfaceApp -> ServiceType NetInterface
  Custom :: Command -> CommandResultParser -> ServiceType CommandResult

deriving stock instance Show (ServiceType result)

deriving stock instance Eq (ServiceType result)

makePrisms ''ServiceType
