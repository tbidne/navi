module Navi.Services.Battery
  ( BatteryToml,
    BatteryToml.batteryCodec,
    toBatteryEvent,
  )
where

import Navi.Data.BoundedN qualified as BoundedN
import Navi.Data.NaviNote (NaviNote (..))
import Navi.Effects (MonadMutRef)
import Navi.Event.Toml qualified as EventToml
import Navi.Event.Types (AnyEvent (..))
import Navi.Prelude
import Navi.Services.Battery.Event qualified as BatteryEvent
import Navi.Services.Battery.Toml (BatteryLevelNoteToml (..), BatteryToml (..))
import Navi.Services.Battery.Toml qualified as BatteryToml
import Navi.Services.Battery.Types (BatteryLevel)
import Optics.Generic (GField (..))
import Optics.Operators ((^.))

toBatteryEvent :: (MonadMutRef m ref) => BatteryToml -> m (AnyEvent ref)
toBatteryEvent MkBatteryToml {alerts, repeatEvent, errorEvent, batteryType} = do
  repeatEvt <- EventToml.mRepeatEvtTomlToVal repeatEvent
  errorNote <- EventToml.mErrorNoteTomlToVal errorEvent
  let evt = BatteryEvent.mkBatteryEvent lvlNoteList batteryType repeatEvt errorNote
  pure $ MkAnyEvent evt
  where
    lvlNoteList = toNote <$> alerts

toNote :: BatteryLevelNoteToml -> (BatteryLevel, NaviNote)
toNote toml =
  ( level,
    MkNaviNote
      summary
      body
      (toml ^. gfield @"mIcon")
      (toml ^. gfield @"urgency")
      (toml ^. gfield @"mTimeout")
  )
  where
    level = toml ^. gfield @"level"
    summary = "Battery"
    body =
      Just $
        "Power is less than "
          <> showt (BoundedN.unBoundedN level)
          <> "%"
