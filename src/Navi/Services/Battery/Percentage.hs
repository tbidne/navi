-- | This module provides a service for alerts related to battery levels.
module Navi.Services.Battery.Percentage
  ( BatteryPercentageToml,
    toEvent,
  )
where

import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as Map
import Navi.Data.NaviNote (NaviNote (..))
import Navi.Data.PollInterval (PollInterval (..))
import Navi.Effects (MonadMutRef)
import Navi.Event.Toml qualified as EventToml
import Navi.Event.Types (AnyEvent (..), ErrorNote, Event (..), RepeatEvent)
import Navi.Prelude
import Navi.Services.Battery.Percentage.Toml (BatteryPercentageNoteToml (..), BatteryPercentageToml)
import Navi.Services.Types (ServiceType (..))
import Numeric.Data.Interval qualified as Interval
import Pythia.Data.Percentage (_MkPercentage)
import Pythia.Services.Battery
  ( Battery (..),
    BatteryConfig (..),
    BatteryStatus (..),
    Percentage (..),
  )

-- | Transforms toml configuration data into an 'AnyEvent'.
toEvent :: (MonadMutRef ref m) => BatteryPercentageToml -> m (AnyEvent ref)
toEvent toml = do
  repeatEvent <- EventToml.mRepeatEventTomlToVal $ toml ^. #repeatEvent
  errorNote <- EventToml.mErrorNoteTomlToVal $ toml ^. #errorNote
  let evt = mkBatteryEvent percentNoteList app pi repeatEvent errorNote
  pure $ MkAnyEvent evt
  where
    percentNoteList = tomlToNote <$> toml ^. #alerts
    app = MkBatteryConfig $ toml ^. #app
    pi = fromMaybe (MkPollInterval 30) (toml ^. #pollInterval)
{-# INLINEABLE toEvent #-}

tomlToNote :: BatteryPercentageNoteToml -> (Percentage, NaviNote)
tomlToNote toml =
  ( percentage,
    MkNaviNote
      summary
      body
      (toml ^. #urgency)
      (toml ^. #mTimeout)
  )
  where
    percentage = toml ^. #percentage
    summary = "Battery Percentage"
    body =
      Just $
        showt (Interval.unLRInterval $ percentage ^. _MkPercentage)
          <> "%"
{-# INLINEABLE tomlToNote #-}

mkBatteryEvent ::
  NonEmpty (Percentage, NaviNote) ->
  BatteryConfig ->
  PollInterval ->
  RepeatEvent ref Battery ->
  ErrorNote ref ->
  Event ref Battery
mkBatteryEvent percentNoteList batteryProgram pi re en =
  MkEvent
    { name = "battery-percentage",
      serviceType = BatteryPercentage batteryProgram,
      pollInterval = pi,
      raiseAlert = lookupPercent percentNoteMap,
      repeatEvent = re,
      errorNote = en
    }
  where
    percentNoteMap = Map.fromList $ NE.toList percentNoteList
{-# INLINEABLE mkBatteryEvent #-}

lookupPercent :: Map Percentage NaviNote -> Battery -> Maybe NaviNote
lookupPercent percentNoteMap state = case state ^. #status of
  Discharging -> Map.lookup (state ^. #percentage) percentNoteMap
  _ -> Nothing
{-# INLINEABLE lookupPercent #-}
