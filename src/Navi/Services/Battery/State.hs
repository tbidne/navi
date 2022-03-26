-- | This module provides a service for alerts related to battery levels.
module Navi.Services.Battery.State
  ( BatteryStateToml,
    BatteryStateToml.batteryStateCodec,
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
import Navi.Services.Battery.State.Toml (BatteryStateNoteToml (..), BatteryStateToml)
import Navi.Services.Battery.State.Toml qualified as BatteryStateToml
import Navi.Services.Types (ServiceType (..))
import Numeric.Data.Interval qualified as Interval
import Optics.Operators ((^.))
import Pythia.Services.Battery.State (BatteryLevel, BatteryStateApp, ChargeStatus (..))
import Pythia.Services.Battery.Types (BatteryState)

-- | Transforms toml configuration data into an 'AnyEvent'.
toEvent :: (MonadMutRef m ref) => BatteryStateToml -> m (AnyEvent ref)
toEvent toml = do
  repeatEvt <- EventToml.mRepeatEvtTomlToVal $ toml ^. #repeatEvent
  errorNote <- EventToml.mErrorNoteTomlToVal $ toml ^. #errorNote
  let evt = mkBatteryEvent lvlNoteList program repeatEvt errorNote
  pure $ MkAnyEvent evt
  where
    lvlNoteList = tomlToNote <$> toml ^. #alerts
    program = toml ^. #program

tomlToNote :: BatteryStateNoteToml -> (BatteryLevel, NaviNote)
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
          <> showt (Interval.unLRInterval level)
          <> "%"

mkBatteryEvent ::
  [(BatteryLevel, NaviNote)] ->
  BatteryStateApp ->
  RepeatEvent ref BatteryState ->
  ErrorNote ref ->
  Event ref BatteryState
mkBatteryEvent lvlNoteList batteryProgram re en =
  MkEvent
    { name = "Battery",
      serviceType = BatteryState batteryProgram,
      raiseAlert = lookupLevel lvlNoteMap,
      repeatEvent = re,
      errorNote = en
    }
  where
    lvlNoteMap = Map.fromList lvlNoteList

lookupLevel :: Map BatteryLevel NaviNote -> BatteryState -> Maybe NaviNote
lookupLevel lvlNoteMap state = case state ^. #status of
  Discharging -> Map.lookup (state ^. #level) lvlNoteMap
  _ -> Nothing
