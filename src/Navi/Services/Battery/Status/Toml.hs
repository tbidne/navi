{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module provides toml configuration for the battery status service.
module Navi.Services.Battery.Status.Toml
  ( BatteryStatusToml (..),
    batteryStatusCodec,
  )
where

import Navi.Data.NaviNote (Timeout)
import Navi.Data.NaviNote qualified as NaviNote
import Navi.Data.PollInterval (PollInterval (..), pollIntervalCodec)
import Navi.Event.Toml (ErrorNoteToml, RepeatEventToml)
import Navi.Event.Toml qualified as EToml
import Navi.Prelude
import Navi.Services.Battery.Common (appCodec)
import Pythia.Services.Battery (BatteryApp (..), RunApp (..))
import Toml (TomlCodec, (.=))
import Toml qualified

-- | TOML for the battery status service.
data BatteryStatusToml = MkBatteryStatusToml
  { -- | Determines how we should query the system for battery information.
    app :: RunApp BatteryApp,
    -- | The poll interval.
    pollInterval :: Maybe PollInterval,
    -- | Determines how we treat repeat alerts.
    repeatEvent :: Maybe RepeatEventToml,
    -- | Determines how we handle errors.
    errorNote :: Maybe ErrorNoteToml,
    -- | The timeout for this alert.
    mTimeout :: Maybe Timeout
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''BatteryStatusToml

-- | Codec for 'BatteryStatusToml'.
batteryStatusCodec :: TomlCodec BatteryStatusToml
batteryStatusCodec =
  MkBatteryStatusToml
    <$> appCodec .= app
    <*> Toml.dioptional pollIntervalCodec .= pollInterval
    <*> Toml.dioptional EToml.repeatEventCodec .= repeatEvent
    <*> Toml.dioptional EToml.errorNoteCodec .= errorNote
    <*> Toml.dioptional NaviNote.timeoutCodec .= mTimeout
{-# INLINEABLE batteryStatusCodec #-}
