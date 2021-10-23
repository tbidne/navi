-- | This module provides the core logic for constructing an event that
-- alerts at different battery statuses.
module Navi.Services.Battery.Status.Event
  ( mkStatusEvent,
  )
where

import DBus.Notify (Icon)
import Data.Attoparsec.Combinator qualified as AP
import Data.Attoparsec.Text (Parser)
import Data.Attoparsec.Text qualified as AP
import Data.Text qualified as T
import Navi.Data.NaviNote (NaviNote, Timeout)
import Navi.Data.NaviNote qualified as NNote
import Navi.Event qualified as Event
import Navi.Event.Types
  ( Command (..),
    ErrorNote (..),
    Event (..),
    EventErr,
    RepeatEvent (..),
  )
import Navi.Prelude
import Navi.Services.Battery.Status.Toml (BatteryStatusNoteToml (..))
import Navi.Services.Battery.Types (BatteryStatus (..), BatteryType (..))
import Optics.Operators ((^.))

-- | Constructs an 'Event' for battery statuses.
mkStatusEvent ::
  BatteryStatusNoteToml ->
  BatteryType ->
  RepeatEvent ref BatteryStatus ->
  ErrorNote ref ->
  Event ref BatteryStatus
mkStatusEvent noteToml batteryType repeatEvent errorNote =
  MkEvent
    { name = "Battery Status",
      command = cmd,
      parser = queryFn,
      raiseAlert = toNote noteToml,
      repeatEvent = repeatEvent,
      errorNote = errorNote
    }
  where
    cmd = typeToCmd batteryType

data BatteryResult
  = None
  | Status BatteryStatus
  deriving (Show)

instance Semigroup BatteryResult where
  Status s <> _ = Status s
  _ <> Status s = Status s
  None <> None = None

instance Monoid BatteryResult where
  mempty = None

toNote :: BatteryStatusNoteToml -> BatteryStatus -> Maybe NaviNote
toNote noteToml status = toNote' timeout $ fromStatus status
  where
    timeout = noteToml ^. #mTimeout
    mChargingImage = noteToml ^. #mChargingImage
    mDischargingImage = noteToml ^. #mChargingImage
    mFullImage = noteToml ^. #mFullImage

    fromStatus Charging = ("Battery charging", mChargingImage)
    fromStatus Discharging = ("Battery discharging", mDischargingImage)
    fromStatus Full = ("Battery full", mFullImage)

toNote' :: Maybe Timeout -> (Text, Maybe Icon) -> Maybe NaviNote
toNote' timeout (msg, icon) =
  Just $
    NNote.MkNaviNote
      { NNote.summary = "Battery Status",
        NNote.body = Just msg,
        NNote.image = icon,
        NNote.urgency = Nothing,
        NNote.timeout = timeout
      }

queryFn :: Text -> Either EventErr BatteryStatus
queryFn infoStr = do
  case parseBattery infoStr of
    Status s -> Right s
    None -> Left $ mkServiceErr $ "Could not parse battery status: " <> show infoStr
  where
    mkServiceErr = Event.MkEventErr "Battery Status" "Parse error" . T.pack

parseBattery :: Text -> BatteryResult
parseBattery txt = foldMap parseLine ts
  where
    ts = T.lines txt

parseLine :: Text -> BatteryResult
parseLine ln = case AP.parseOnly parseState ln of
  Right s -> Status s
  Left _ -> None

parseState :: Parser BatteryStatus
parseState =
  AP.skipSpace
    *> AP.string "state:"
    *> AP.skipSpace
    *> (discharging <|> charging <|> full)
  where
    discharging = AP.string "discharging" $> Discharging <* rest
    charging = AP.string "charging" $> Charging <* rest
    full = AP.string "fully-charged" $> Full <* rest
    rest = AP.skipSpace *> AP.endOfInput

typeToCmd :: BatteryType -> Command
typeToCmd UPower = MkCommand "upower -i `upower -e | grep 'BAT'`"
typeToCmd (Custom c) = MkCommand c
