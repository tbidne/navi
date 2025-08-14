-- | Provides common functionality for battery services.
module Navi.Services.Battery.Common
  ( batteryAppDecoder,
  )
where

import Navi.Prelude
import Pythia.Services.Battery
  ( BatteryApp
      ( BatteryAppAcpi,
        BatteryAppSysFs,
        BatteryAppUPower
      ),
  )

-- | TOML decoder for 'BatteryApp'.
--
-- @since 0.1
batteryAppDecoder :: Decoder BatteryApp
batteryAppDecoder =
  tomlDecoder >>= \case
    "acpi" -> pure BatteryAppAcpi
    "sysfs" -> pure BatteryAppSysFs
    "upower" -> pure BatteryAppUPower
    bad ->
      fail
        $ unpackText
        $ mconcat
          [ "Unexpected battery app: ",
            bad,
            ". Expected one of <acpi | sysfs | upower>."
          ]
