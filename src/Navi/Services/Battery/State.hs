-- | This module provides a service for alerts related to battery levels.
module Navi.Services.Battery.State
  ( BatteryLevelToml,
    BatteryLevelToml.batteryLevelCodec,
    toBatteryLevelEvent,
  )
where

import Data.Map (Map)
import Data.Map qualified as Map
import Navi.Data.NaviNote (NaviNote (..))
import Navi.Effects (MonadMutRef)
import Navi.Event.Toml qualified as EventToml
import Navi.Event.Types (AnyEvent (..), ErrorNote, Event (..), RepeatEvent)
import Navi.Prelude
import Navi.Services.Battery.State.Toml (BatteryLevelNoteToml (..), BatteryLevelToml)
import Navi.Services.Battery.State.Toml qualified as BatteryLevelToml
import Navi.Services.Types (ServiceType (BatteryState))
import Optics.Operators ((^.))
import Smart.Data.Math.BoundedNat (BoundedNat (..))
import System.Info.Services.Battery.State (BatteryLevel, ChargeStatus (..), Program)
import System.Info.Services.Battery.Types (BatteryState)

-- | Transforms toml configuration data into an 'AnyEvent'.
toBatteryLevelEvent :: (MonadMutRef m ref) => BatteryLevelToml -> m (AnyEvent ref)
toBatteryLevelEvent toml = do
  repeatEvt <- EventToml.mRepeatEvtTomlToVal $ toml ^. #repeatEvent
  errorNote <- EventToml.mErrorNoteTomlToVal $ toml ^. #errorNote
  let evt = mkBatteryEvent lvlNoteList program repeatEvt errorNote
  pure $ MkAnyEvent evt
  where
    lvlNoteList = tomlToNote <$> toml ^. #alerts
    program = toml ^. #program

tomlToNote :: BatteryLevelNoteToml -> (BatteryLevel, NaviNote)
tomlToNote toml =
  ( level,
    MkNaviNote
      summary
      body
      (toml ^. #mIcon)
      (toml ^. #urgency)
      (toml ^. #mTimeout)
  )
  where
    level = toml ^. #level
    summary = "Battery"
    body =
      Just $
        "Power is less than "
          <> showt (unBoundedNat level)
          <> "%"

mkBatteryEvent ::
  [(BatteryLevel, NaviNote)] ->
  Program ->
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
