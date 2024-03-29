-- | This module provides a service for alerts related to battery levels.
module Navi.Services.Battery.Percentage
  ( BatteryPercentageToml,
    toEvent,
  )
where

import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as Map
import Navi.Data.NaviNote (NaviNote (MkNaviNote))
import Navi.Data.PollInterval (PollInterval (MkPollInterval))
import Navi.Event.Toml qualified as EventToml
import Navi.Event.Types
  ( AnyEvent (MkAnyEvent),
    ErrorNote,
    Event
      ( MkEvent,
        errorNote,
        name,
        pollInterval,
        raiseAlert,
        repeatEvent,
        serviceType
      ),
    RepeatEvent,
  )
import Navi.Prelude
import Navi.Services.Battery.Percentage.Toml
  ( BatteryPercentageNoteToml,
    BatteryPercentageToml,
  )
import Navi.Services.Types (ServiceType (BatteryPercentage))
import Pythia.Data.Percentage qualified as Percentage
import Pythia.Services.Battery
  ( Battery,
    BatteryApp,
    BatteryStatus (Discharging),
    Percentage,
  )

-- | Transforms toml configuration data into an 'AnyEvent'.
toEvent :: (MonadIORef m) => BatteryPercentageToml -> m AnyEvent
toEvent toml = do
  repeatEvent <- EventToml.mRepeatEventTomlToVal $ toml ^. #repeatEvent
  errorNote <- EventToml.mErrorNoteTomlToVal $ toml ^. #errorNote
  let evt = mkBatteryEvent percentNoteList app pi repeatEvent errorNote
  pure $ MkAnyEvent evt
  where
    percentNoteList = tomlToNote <$> toml ^. #alerts
    app = toml ^. #app
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
      Just
        $ showt (Percentage.unPercentage percentage)
        <> "%"

mkBatteryEvent ::
  NonEmpty (Percentage, NaviNote) ->
  BatteryApp ->
  PollInterval ->
  RepeatEvent Battery ->
  ErrorNote ->
  Event Battery
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

lookupPercent :: Map Percentage NaviNote -> Battery -> Maybe NaviNote
lookupPercent percentNoteMap state = case state ^. #status of
  Discharging -> Map.lookup (state ^. #percentage) percentNoteMap
  _ -> Nothing
