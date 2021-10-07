module Navi.Config.Toml
  ( ConfigToml (..),
    configCodec,
  )
where

import Navi.Data.NonNegative (NonNegative)
import Navi.Data.NonNegative qualified as NonNegative
import Navi.Prelude
import Navi.Services.Battery.Toml as BatteryToml
import Navi.Services.Custom.Multiple.Toml as MultipleToml
import Navi.Services.Custom.Single.Toml as SingleToml
import Toml (TomlCodec, (.=))
import Toml qualified

data ConfigToml = MkConfigToml
  { pollToml :: NonNegative,
    singleToml :: [SingleToml],
    multipleToml :: [MultipleToml],
    batteryToml :: Maybe BatteryToml
  }
  deriving (Show)

configCodec :: TomlCodec ConfigToml
configCodec =
  MkConfigToml
    <$> NonNegative.nonNegativeCodec "poll-interval" .= pollToml
    <*> Toml.list SingleToml.singleCodec "single" .= singleToml
    <*> Toml.list MultipleToml.multipleCodec "multiple" .= multipleToml
    <*> Toml.dioptional (Toml.table BatteryToml.batteryCodec "battery") .= batteryToml
