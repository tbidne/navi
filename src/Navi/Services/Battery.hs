module Navi.Services.Battery
  ( mkBatteryEvent,
    batteryNNote,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (Exception (..))
import Control.Exception qualified as Except
import DBus.Notify
  ( Body (..),
    Hint (..),
    Icon (..),
    Note (..),
    Timeout (..),
    UrgencyLevel (..),
  )
import Data.Attoparsec.Combinator qualified as AP
import Data.Attoparsec.Text (Parser)
import Data.Attoparsec.Text qualified as AP
import Data.Functor (($>))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Navi.Class.Service (Service (..))
import Navi.Data.Event (ErrorEvent (..), Event (..), RepeatEvent (..), Trigger (..))
import Navi.Data.Event qualified as Event
import Navi.Data.ServiceException qualified as ServiceEx
import Navi.Data.Sorted (Sorted (..))
import Navi.Data.Sorted qualified as Sorted
import Navi.Utils qualified as NSUtils

mkBatteryEvent ::
  Map Int Note ->
  RepeatEvent (Maybe Int) ->
  ErrorEvent ->
  Event (Maybe Int)
mkBatteryEvent lvlNoteMap repeatEvt = MkEvent trigger getNote repeatEvt
  where
    batLvls = Sorted.unsafeMkSorted $ Map.keys lvlNoteMap
    trigger = MkTrigger $ batteryTrigger batLvls repeatEvt
    getNote = (=<<) (`Map.lookup` lvlNoteMap)

newtype BatteryEx = MkBatteryEx String
  deriving (Show)

instance Service BatteryEx where
  name = "Battery"

instance Exception BatteryEx where
  displayException (MkBatteryEx err) = err
  toException = ServiceEx.toServiceEx
  fromException = ServiceEx.fromServiceEx

batteryTrigger :: Sorted Int -> RepeatEvent (Maybe Int) -> IO (Maybe Int)
batteryTrigger alertLvls repeatEvt = do
  infoStr <- T.pack <$> Event.execStr cmd
  case parseBattery infoStr of
    Both currLvl state
      -- We are discharging so we need to check more conditions to see what to
      -- do next.
      | state == Discharging -> do
        let mLub = Sorted.leastUpperBound currLvl alertLvls
        case mLub of
          -- If our current alert level is not < any defined alert level
          -- then we have nothing to do other than possibly reset the
          -- cached trigger.
          Nothing -> NSUtils.updatePrevTrigger repeatEvt Nothing $> Nothing
          -- We do have an alert violation.
          Just violatedAlertLvl -> do
            let jViolatedAlertLvl = Just violatedAlertLvl
            NSUtils.triggerIfNotRepeat repeatEvt Nothing jViolatedAlertLvl
      -- If we are currently charging then there is nothing to do other than
      -- possibly reset the cached repeatEvt trigger.
      | otherwise -> NSUtils.updatePrevTrigger repeatEvt Nothing $> Nothing
    None -> throwErr $ "Could not parse battery percent and state: " <> show infoStr
    Percent _ -> throwErr $ "Could not parse battery state: " <> show infoStr
    State _ -> throwErr $ "Could not parse battery percent: " <> show infoStr
  where
    cmd = "upower -i `upower -e | grep 'BAT'`"
    throwErr str = Except.throwIO $ MkBatteryEx str

batteryNNote :: Int -> Maybe Icon -> UrgencyLevel -> Timeout -> Note
batteryNNote n icon urgency = Event.mkNote icon summary body hints
  where
    summary = "Battery"
    body = Just (Text ("Power is less than " <> show n <> "%"))
    hints = [Urgency urgency]

data BatteryResult
  = None
  | Percent Int
  | State BatteryState
  | Both Int BatteryState
  deriving (Show)

instance Semigroup BatteryResult where
  Both n s <> _ = Both n s
  _ <> Both n s = Both n s
  None <> r = r
  l <> None = l
  Percent n <> State s = Both n s
  State s <> Percent n = Both n s
  l <> _ = l

instance Monoid BatteryResult where
  mempty = None

parseBattery :: Text -> BatteryResult
parseBattery txt = foldMap parseLine ts
  where
    ts = T.lines txt

parseLine :: Text -> BatteryResult
parseLine ln = case AP.parseOnly parseState ln of
  Right s -> State s
  Left _ -> case AP.parseOnly parsePercent ln of
    Right n -> Percent n
    Left _ -> None

parsePercent :: Parser Int
parsePercent =
  AP.skipSpace
    *> AP.string "percentage:"
    *> AP.skipSpace
    *> AP.decimal
    <* end
  where
    end = AP.char '%' *> AP.skipSpace

data BatteryState
  = Charging
  | Discharging
  | Full
  deriving (Eq, Show)

parseState :: Parser BatteryState
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