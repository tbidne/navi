{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module provides toml configuration for the battery status service.
module Navi.Services.Battery.Status.Toml
  ( BatteryStatusToml (..),
    BatteryStatusNoteToml (..),
    batteryStatusCodec,
  )
where

import Navi.Data.NaviNote (Timeout)
import Navi.Data.NaviNote qualified as NaviNote
import Navi.Event.Toml (ErrorNoteToml, RepeatEvtToml)
import Navi.Event.Toml qualified as EToml
import Navi.Prelude
import Pythia.Data.RunApp (RunApp (..))
import Pythia.Services.Battery (BatteryApp (..), BatteryConfig (..))
import Toml (TomlCodec, (.=))
import Toml qualified

-- | TOML for the battery status notification.
newtype BatteryStatusNoteToml = MkBatteryStatusNoteToml
  { -- | The timeout for this alert.
    mTimeout :: Maybe Timeout
  }
  deriving (Show)

makeFieldLabelsNoPrefix ''BatteryStatusNoteToml

-- | TOML for the battery status service.
data BatteryStatusToml = MkBatteryStatusToml
  { -- | Determines how we should query the system for battery information.
    program :: BatteryConfig,
    -- | Determines how we treat repeat alerts.
    repeatEvent :: Maybe RepeatEvtToml,
    -- | Determines how we handle errors.
    errorNote :: Maybe ErrorNoteToml,
    -- | The alert for this service.
    note :: BatteryStatusNoteToml
  }
  deriving (Show)

makeFieldLabelsNoPrefix ''BatteryStatusToml

-- | Codec for 'BatteryStatusToml'.
batteryStatusCodec :: TomlCodec BatteryStatusToml
batteryStatusCodec =
  MkBatteryStatusToml
    <$> programCodec .= program
    <*> Toml.dioptional EToml.repeatEvtCodec .= repeatEvent
    <*> Toml.dioptional EToml.errorNoteCodec .= errorNote
    <*> batteryStatusNoteCodec .= note

batteryStatusNoteCodec :: TomlCodec BatteryStatusNoteToml
batteryStatusNoteCodec =
  MkBatteryStatusNoteToml
    <$> Toml.dioptional NaviNote.timeoutCodec .= mTimeout

programCodec :: TomlCodec BatteryConfig
programCodec =
  Toml.textBy showBatteryType parseBatteryType "type"
    <|> pure (MkBatteryConfig $ Single BatterySysFs)
  where
    showBatteryType (configToApp -> Single BatteryAcpi) = "acpi"
    showBatteryType (configToApp -> Single BatterySysFs) = "sysfs"
    showBatteryType (configToApp -> Single BatteryUPower) = "upower"
    showBatteryType _ = ""
    parseBatteryType "acpi" = Right (mkSingleApp BatteryAcpi)
    parseBatteryType "sysfs" = Right (mkSingleApp BatterySysFs)
    parseBatteryType "upower" = Right (mkSingleApp BatteryUPower)
    parseBatteryType t = Left t

-- TODO: allow for no option

mkSingleApp :: BatteryApp -> BatteryConfig
mkSingleApp = MkBatteryConfig . Single

configToApp :: BatteryConfig -> RunApp BatteryApp
configToApp (MkBatteryConfig app) = app
