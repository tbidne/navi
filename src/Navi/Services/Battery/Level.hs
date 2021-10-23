-- | This module provides a service for alerts related to battery levels.
module Navi.Services.Battery.Level
  ( BatteryLevelToml,
    BatteryLevelToml.batteryLevelCodec,
    toBatteryLevelEvent,
  )
where

import Navi.Data.BoundedN qualified as BoundedN
import Navi.Data.NaviNote (NaviNote (..))
import Navi.Effects (MonadMutRef)
import Navi.Event.Toml qualified as EventToml
import Navi.Event.Types (AnyEvent (..))
import Navi.Prelude
import Navi.Services.Battery.Level.Event qualified as BatteryLevelEvent
import Navi.Services.Battery.Level.Toml (BatteryLevelNoteToml (..), BatteryLevelToml (..))
import Navi.Services.Battery.Level.Toml qualified as BatteryLevelToml
import Navi.Services.Battery.Types (BatteryLevel)
import Optics.Operators ((^.))

-- | Transforms toml configuration data into an 'AnyEvent'.
toBatteryLevelEvent :: (MonadMutRef m ref) => BatteryLevelToml -> m (AnyEvent ref)
toBatteryLevelEvent toml = do
  repeatEvt <- EventToml.mRepeatEvtTomlToVal $ toml ^. #repeatEvent
  errorNote <- EventToml.mErrorNoteTomlToVal $ toml ^. #errorNote
  let evt = BatteryLevelEvent.mkBatteryEvent lvlNoteList batteryType repeatEvt errorNote
  pure $ MkAnyEvent evt
  where
    lvlNoteList = toNote <$> toml ^. #alerts
    batteryType = toml ^. #batteryType

toNote :: BatteryLevelNoteToml -> (BatteryLevel, NaviNote)
toNote toml =
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
          <> showt (BoundedN.unBoundedN level)
          <> "%"
