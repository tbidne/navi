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

_BatteryPercentage :: (result ~ Battery) => Prism' (ServiceType result) BatteryApp
_BatteryPercentage =
  prism
    BatteryPercentage
    ( \case
        BatteryPercentage x -> Right x
    )
{-# INLINE _BatteryPercentage #-}

_BatteryStatus :: (result ~ BatteryStatus) => Prism' (ServiceType result) BatteryApp
_BatteryStatus =
  prism
    BatteryStatus
    ( \case
        BatteryStatus x -> Right x
    )
{-# INLINE _BatteryStatus #-}

_Custom :: (result ~ CommandResult) => Prism' (ServiceType result) (Command, CommandResultParser)
_Custom =
  prism
    (uncurry Custom)
    ( \case
        Custom x y -> Right (x, y)
    )
{-# INLINE _Custom #-}

_NetworkInterface :: (result ~ NetInterface) => Prism' (ServiceType result) (Device, NetInterfaceApp)
_NetworkInterface =
  prism
    (uncurry NetworkInterface)
    ( \case
        NetworkInterface x y -> Right (x, y)
    )
{-# INLINE _NetworkInterface #-}
