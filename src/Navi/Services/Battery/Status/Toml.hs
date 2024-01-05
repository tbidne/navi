{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module provides toml configuration for the battery status service.
module Navi.Services.Battery.Status.Toml
  ( BatteryStatusToml (..),
  )
where

import Navi.Data.NaviNote (Timeout, timeoutOptDecoder)
import Navi.Data.PollInterval (PollInterval (..), pollIntervalOptDecoder)
import Navi.Event.Toml
  ( ErrorNoteToml,
    RepeatEventToml,
    errorNoteOptDecoder,
    repeatEventOptDecoder,
  )
import Navi.Prelude
import Navi.Services.Battery.Common (batteryAppDecoder)
import Pythia.Services.Battery (BatteryApp)

-- | TOML for the battery status service.
data BatteryStatusToml = MkBatteryStatusToml
  { -- | Determines how we should query the system for battery information.
    app :: BatteryApp,
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

-- | @since 0.1
instance DecodeTOML BatteryStatusToml where
  tomlDecoder =
    MkBatteryStatusToml
      <$> getFieldWith batteryAppDecoder "app"
      <*> pollIntervalOptDecoder
      <*> repeatEventOptDecoder
      <*> errorNoteOptDecoder
      <*> timeoutOptDecoder
