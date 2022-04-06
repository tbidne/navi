-- | This module provides a service for alerts related to battery levels.
module Navi.Services.Battery.Percentage
  ( BatteryPercentageToml,
    BatteryPercentageToml.batteryPercentageCodec,
    toEvent,
  )
where

import Data.Map (Map)
import Data.Map qualified as Map
import Navi.Data.NaviNote (NaviNote (..))
import Navi.Effects (MonadMutRef)
import Navi.Event.Toml qualified as EventToml
import Navi.Event.Types (AnyEvent (..), ErrorNote, Event (..), RepeatEvent)
import Navi.Prelude
import Navi.Services.Battery.Percentage.Toml (BatteryPercentageNoteToml (..), BatteryPercentageToml)
import Navi.Services.Battery.Percentage.Toml qualified as BatteryPercentageToml
import Navi.Services.Types (ServiceType (..))
import Numeric.Data.Interval qualified as Interval
import Pythia.Services.Battery
  ( Battery (..),
    BatteryConfig (..),
    BatteryPercentage (..),
    BatteryStatus (..),
  )

-- | Transforms toml configuration data into an 'AnyEvent'.
toEvent :: (MonadMutRef m ref) => BatteryPercentageToml -> m (AnyEvent ref)
toEvent toml = do
  repeatEvt <- EventToml.mRepeatEvtTomlToVal $ toml ^. #repeatEvent
  errorNote <- EventToml.mErrorNoteTomlToVal $ toml ^. #errorNote
  let evt = mkBatteryEvent percentNoteList program repeatEvt errorNote
  pure $ MkAnyEvent evt
  where
    percentNoteList = tomlToNote <$> toml ^. #alerts
    program = toml ^. #program

tomlToNote :: BatteryPercentageNoteToml -> (BatteryPercentage, NaviNote)
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
        "Power is less than "
          <> showt (Interval.unLRInterval $ percentage ^. #unBatteryPercentage)
          <> "%"

mkBatteryEvent ::
  [(BatteryPercentage, NaviNote)] ->
  BatteryConfig ->
  RepeatEvent ref Battery ->
  ErrorNote ref ->
  Event ref Battery
mkBatteryEvent percentNoteList batteryProgram re en =
  MkEvent
    { name = "Battery",
      serviceType = BatteryPercentage batteryProgram,
      raiseAlert = lookupPercent percentNoteMap,
      repeatEvent = re,
      errorNote = en
    }
  where
    percentNoteMap = Map.fromList percentNoteList

lookupPercent :: Map BatteryPercentage NaviNote -> Battery -> Maybe NaviNote
lookupPercent percentNoteMap state = case state ^. #status of
  Discharging -> Map.lookup (state ^. #percentage) percentNoteMap
  _ -> Nothing
