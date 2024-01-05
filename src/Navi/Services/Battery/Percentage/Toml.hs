{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module provides toml configuration for the battery percentage service.
module Navi.Services.Battery.Percentage.Toml
  ( BatteryPercentageToml (..),
    BatteryPercentageNoteToml (..),
  )
where

import DBus.Notify (UrgencyLevel)
import Navi.Data.NaviNote (Timeout, timeoutOptDecoder)
import Navi.Data.PollInterval (PollInterval, pollIntervalOptDecoder)
import Navi.Event.Toml
  ( ErrorNoteToml,
    RepeatEventToml,
    errorNoteOptDecoder,
    repeatEventOptDecoder,
  )
import Navi.Prelude
import Navi.Services.Battery.Common (batteryAppDecoder)
import Navi.Utils (urgencyLevelOptDecoder)
import Pythia.Data.Percentage qualified as Percentage
import Pythia.Services.Battery (BatteryApp (..), Percentage (..))

-- | TOML for each individual battery percentage.
data BatteryPercentageNoteToml = MkBatteryPercentageNoteToml
  { -- | The percentage for this alert.
    percentage :: Percentage,
    -- | The urgency for this alert.
    urgency :: Maybe UrgencyLevel,
    -- | The timeout for this alert.
    mTimeout :: Maybe Timeout
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''BatteryPercentageNoteToml

-- | @since 0.1
instance DecodeTOML BatteryPercentageNoteToml where
  tomlDecoder =
    MkBatteryPercentageNoteToml
      <$> percentageDecoder
      <*> urgencyLevelOptDecoder
      <*> timeoutOptDecoder

percentageDecoder :: Decoder Percentage
percentageDecoder = getFieldWith decoder "percent"
  where
    decoder =
      tomlDecoder >>= \x ->
        case Percentage.mkPercentage x of
          Just n -> pure n
          Nothing ->
            fail
              $ unpack
              $ concat
                [ "Unexpected percent: ",
                  showt x,
                  ". Expected integer in [0, 100]."
                ]

-- | TOML for the battery percentage service.
data BatteryPercentageToml = MkBatteryPercentageToml
  { -- | All alerts for this service.
    alerts :: NonEmpty BatteryPercentageNoteToml,
    -- | The poll interval.
    pollInterval :: Maybe PollInterval,
    -- | Determines how we treat repeat alerts.
    repeatEvent :: Maybe RepeatEventToml,
    -- | Determines how we handle errors.
    errorNote :: Maybe ErrorNoteToml,
    -- | Determines how we should query the system for battery information.
    app :: BatteryApp
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''BatteryPercentageToml

-- | @since 0.1
instance DecodeTOML BatteryPercentageToml where
  tomlDecoder =
    MkBatteryPercentageToml
      <$> getFieldWith tomlDecoder "alert"
      <*> pollIntervalOptDecoder
      <*> repeatEventOptDecoder
      <*> errorNoteOptDecoder
      <*> getFieldWith batteryAppDecoder "app"
