-- | This module provides a service for alerts related to battery levels.
module Navi.Services.Battery.Percentage
  ( BatteryPercentageToml,
    BatteryPercentageToml.batteryPercentageCodec,
    toEvent,
  )
where

import Data.List.NonEmpty qualified as NE
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
toEvent :: (MonadMutRef ref m) => BatteryPercentageToml -> m (AnyEvent ref)
toEvent toml = do
  repeatEvt <- EventToml.mRepeatEvtTomlToVal $ toml ^. #repeatEvent
  errorNote <- EventToml.mErrorNoteTomlToVal $ toml ^. #errorNote
  let evt = mkBatteryEvent percentNoteList app repeatEvt errorNote
  pure $ MkAnyEvent evt
  where
    percentNoteList = tomlToNote <$> toml ^. #alerts
    app = MkBatteryConfig $ toml ^. #app

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
  NonEmpty (BatteryPercentage, NaviNote) ->
  BatteryConfig ->
  RepeatEvent ref Battery ->
  ErrorNote ref ->
  Event ref Battery
mkBatteryEvent percentNoteList batteryProgram re en =
  MkEvent
    { name = "Battery Percentage",
      serviceType = BatteryPercentage batteryProgram,
      raiseAlert = lookupPercent percentNoteMap,
      repeatEvent = re,
      errorNote = en
    }
  where
    percentNoteMap = Map.fromList $ NE.toList percentNoteList

lookupPercent :: Map BatteryPercentage NaviNote -> Battery -> Maybe NaviNote
lookupPercent percentNoteMap state = case state ^. #status of
  Discharging -> Map.lookup (state ^. #percentage) percentNoteMap
  _ -> Nothing
