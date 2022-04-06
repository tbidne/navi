-- | This module provides a service for alerts related to battery levels.
module Navi.Services.Battery.State
  ( BatteryPercentageToml,
    BatteryPercentageToml.batteryStateCodec,
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
import Navi.Services.Battery.State.Toml (BatteryPercentageNoteToml (..), BatteryPercentageToml)
import Navi.Services.Battery.State.Toml qualified as BatteryPercentageToml
import Navi.Services.Types (ServiceType (..))
import Numeric.Data.Interval qualified as Interval
import Optics.Core ((^.))
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
  let evt = mkBatteryEvent lvlNoteList program repeatEvt errorNote
  pure $ MkAnyEvent evt
  where
    lvlNoteList = tomlToNote <$> toml ^. #alerts
    program = toml ^. #program

tomlToNote :: BatteryPercentageNoteToml -> (BatteryPercentage, NaviNote)
tomlToNote toml =
  ( level,
    MkNaviNote
      summary
      body
      (toml ^. #urgency)
      (toml ^. #mTimeout)
  )
  where
    level = toml ^. #level
    summary = "Battery State"
    body =
      Just $
        "Power is less than "
          <> showt (Interval.unLRInterval $ level ^. #unBatteryPercentage)
          <> "%"

mkBatteryEvent ::
  [(BatteryPercentage, NaviNote)] ->
  BatteryConfig ->
  RepeatEvent ref Battery ->
  ErrorNote ref ->
  Event ref Battery
mkBatteryEvent lvlNoteList batteryProgram re en =
  MkEvent
    { name = "Battery",
      serviceType = BatteryPercentage batteryProgram,
      raiseAlert = lookupLevel lvlNoteMap,
      repeatEvent = re,
      errorNote = en
    }
  where
    lvlNoteMap = Map.fromList lvlNoteList

lookupLevel :: Map BatteryPercentage NaviNote -> Battery -> Maybe NaviNote
lookupLevel lvlNoteMap state = case state ^. #status of
  Discharging -> Map.lookup (state ^. #percentage) lvlNoteMap
  _ -> Nothing
