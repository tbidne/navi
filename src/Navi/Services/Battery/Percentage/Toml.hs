{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module provides toml configuration for the battery percentage service.
module Navi.Services.Battery.Percentage.Toml
  ( BatteryPercentageToml (..),
    BatteryPercentageNoteToml (..),
    PercentageData (..),
  )
where

import DBus.Notify (UrgencyLevel)
import Data.List.NonEmpty qualified as NE
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Navi.Data.NaviNote (Timeout, timeoutOptDecoder)
import Navi.Data.PollInterval (PollInterval, pollIntervalOptDecoder)
import Navi.Event.Toml
  ( ErrorNoteToml,
    MultiRepeatEventToml (MultiSomeRepeatsToml),
    errorNoteOptDecoder,
    multiRepeatEventOptDecoder,
  )
import Navi.Prelude
import Navi.Services.Battery.Common (batteryAppDecoder)
import Navi.Utils (urgencyLevelOptDecoder)
import Pythia.Data.Percentage (mkPercentage, _MkPercentage)
import Pythia.Data.Percentage qualified as Percentage
import Pythia.Services.Battery (BatteryApp, Percentage)

-- | Exact percentage or range. Equality/Ord is based on the _lower_ part of
-- the range only. This allows us to have a total order and make lookups
-- sensible.
data PercentageData
  = -- | Exact percentage.
    PercentageExact Percentage
  | -- | Percentage range. The LHS should be <= RHS.
    PercentageRange Percentage Percentage
  deriving stock (Show)

instance Eq PercentageData where
  x == y = toPercentage x == toPercentage y

instance Ord PercentageData where
  x <= y = toPercentage x <= toPercentage y

toPercentage :: PercentageData -> Percentage
toPercentage (PercentageExact p) = p
toPercentage (PercentageRange l _) = l

-- | TOML for each individual battery percentage.
data BatteryPercentageNoteToml = MkBatteryPercentageNoteToml
  { -- | The percentage (range) for this alert.
    percentage :: PercentageData,
    -- | The urgency for this alert.
    urgency :: Maybe UrgencyLevel,
    -- | The timeout for this alert.
    mTimeout :: Maybe Timeout
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''BatteryPercentageNoteToml

-- | @since 0.1
instance DecodeTOML BatteryPercentageNoteToml where
  tomlDecoder = do
    percentage <- percentageDataDecoder
    mTimeout <- timeoutOptDecoder
    urgency <- urgencyLevelOptDecoder
    pure
      $ MkBatteryPercentageNoteToml
        { percentage,
          mTimeout,
          urgency
        }

percentageDataDecoder :: Decoder PercentageData
percentageDataDecoder = do
  mExact <- exactDecoder
  mRange <- rangeDecoder
  case (mExact, mRange) of
    (Just exact, Nothing) -> pure exact
    (Nothing, Just range) -> pure range
    (Just _, Just _) -> fail "Expected 'percent' or ('lower' and 'upper'), not both!"
    (Nothing, Nothing) -> fail "Expected 'percent' or ('lower' and 'upper'), received neither!"

exactDecoder :: Decoder (Maybe PercentageData)
exactDecoder = fmap PercentageExact <$> getFieldOptWith percentageDecoder "percent"

rangeDecoder :: Decoder (Maybe PercentageData)
rangeDecoder = do
  mLow <- getFieldOptWith percentageDecoder "lower"
  mHigh <- getFieldOptWith percentageDecoder "upper"

  case (mLow, mHigh) of
    (Just low, Just high) -> do
      let msg =
            unpackText
              $ mconcat
                [ "Percentage 'upper = ",
                  display high,
                  "' < 'lower = ",
                  display low,
                  "'."
                ]
      when (high < low) $ fail msg

      pure $ Just $ PercentageRange low high
    _ -> pure Nothing

percentageDecoder :: Decoder Percentage
percentageDecoder =
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
    -- | Determines how we should query the system for battery information.
    app :: BatteryApp,
    -- | Determines how we handle errors.
    errorNote :: Maybe ErrorNoteToml,
    -- | The poll interval.
    pollInterval :: Maybe PollInterval,
    -- | Determines how we treat repeat alerts. We are given exact percentages,
    -- so if the user wants a repeatEvent to apply to a range, they should
    -- give the _lower_ bound.
    repeatEvent :: Maybe (MultiRepeatEventToml Percentage)
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''BatteryPercentageToml

-- | @since 0.1
instance DecodeTOML BatteryPercentageToml where
  tomlDecoder = do
    alerts <- getFieldWith tomlDecoder "alert"
    app <- getFieldWith batteryAppDecoder "app"
    errorNote <- errorNoteOptDecoder
    pollInterval <- pollIntervalOptDecoder
    repeatEvent <- multiRepeatEventOptDecoder decodePercentage

    case repeatEvent of
      Just (MultiSomeRepeatsToml percentRefs) -> do
        let alertsPercents = mkAlertPercents alerts
            d = Set.difference percentRefs alertsPercents
            msg =
              mconcat
                [ "Found repeat-events that referenced non-extant alert ",
                  "percentages. All references should correspond to an alert ",
                  "'percent' or 'lower': ",
                  showSet d
                ]
        unless (Set.null d) $ fail msg
      _ -> pure ()

    pure
      $ MkBatteryPercentageToml
        { alerts,
          app,
          errorNote,
          pollInterval,
          repeatEvent
        }
    where
      decodePercentage (Integer s) = case mkPercentage (fromIntegral s) of
        Just p -> pure p
        Nothing -> fail $ "Failed to parse percentage: " ++ show s
      decodePercentage other = typeMismatch other

      mkAlertPercents :: NonEmpty BatteryPercentageNoteToml -> Set Percentage
      mkAlertPercents =
        Set.fromList
          . NE.toList
          . fmap (toPercentage . view #percentage)

      showSet :: Set Percentage -> String
      showSet =
        unpack
          . (<> ".")
          . T.intercalate ", "
          . fmap (showt . view _MkPercentage)
          . Set.toList
