-- | Provides common functionality for battery services.
module Navi.Services.Battery.Common
  ( appCodec,
  )
where

import Navi.Prelude
import Pythia.Services.Battery (BatteryApp (..), RunApp (..))
import Toml (TomlCodec)
import Toml qualified

-- | Codec for 'RunApp' 'BatteryApp'.
appCodec :: TomlCodec (RunApp BatteryApp)
appCodec = Toml.dimap f g mappCodec
  where
    f Many = Nothing
    f (Single x) = Just x
    g Nothing = Many
    g (Just x) = Single x

mappCodec :: TomlCodec (Maybe BatteryApp)
mappCodec =
  Toml.dioptional $ Toml.textBy showBatteryType parseBatteryType "app"
  where
    showBatteryType BatteryAcpi = "acpi"
    showBatteryType BatterySysFs = "sysfs"
    showBatteryType BatteryUPower = "upower"
    parseBatteryType "acpi" = Right BatteryAcpi
    parseBatteryType "sysfs" = Right BatterySysFs
    parseBatteryType "upower" = Right BatteryUPower
    parseBatteryType t = Left t
