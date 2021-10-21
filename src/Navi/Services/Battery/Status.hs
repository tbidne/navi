module Navi.Services.Battery.Status
  ( BatteryStatusToml,
    BatteryStatusToml.batteryStatusCodec,
    toBatteryStatusEvent,
  )
where

import Navi.Effects (MonadMutRef)
import Navi.Event.Toml qualified as EventToml
import Navi.Event.Types (AnyEvent (..))
import Navi.Prelude
import Navi.Services.Battery.Status.Event qualified as BatteryStatusEvent
import Navi.Services.Battery.Status.Toml (BatteryStatusToml (..))
import Navi.Services.Battery.Status.Toml qualified as BatteryStatusToml
import Optics.Generic (GField (..))
import Optics.Operators ((^.))

toBatteryStatusEvent :: (MonadMutRef m ref) => BatteryStatusToml -> m (AnyEvent ref)
toBatteryStatusEvent toml = do
  repeatEvt <- EventToml.mRepeatEvtTomlToVal $ toml ^. gfield @"repeatEvent"
  errorNote <- EventToml.mErrorNoteTomlToVal $ toml ^. gfield @"errorNote"
  let evt = BatteryStatusEvent.mkStatusEvent note batteryType repeatEvt errorNote
  pure $ MkAnyEvent evt
  where
    batteryType = toml ^. gfield @"batteryType"
    note = toml ^. gfield @"note"
