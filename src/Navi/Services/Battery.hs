module Navi.Services.Battery
  ( mkBatteryEvent,
    batteryNNote,
  )
where

import Control.Applicative ((<|>))
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
import Navi.Data.Event (Command (..), ErrorEvent (..), Event (..), RepeatEvent (..))
import Navi.Data.Event qualified as Event
import Navi.Data.Sorted qualified as Sorted
import Navi.Services.Types (ServiceErr (..))

mkBatteryEvent :: [(Int, Note)] -> RepeatEvent BatteryState -> ErrorEvent -> Event
mkBatteryEvent lvlNoteList = Event.mkEvent cmd parserFn lvlNoteMap lookupFn
  where
    lvlNoteMap = Map.fromList lvlNoteList
    cmd = MkCommand "upower -i `upower -e | grep 'BAT'`"
    parserFn = queryFn
    lookupFn = toNote

queryFn :: Text -> Either ServiceErr BatteryState
queryFn infoStr = do
  case parseBattery infoStr of
    Both s -> Right s
    None -> Left $ mkServiceErr $ "Could not parse battery percent and state: " <> show infoStr
    Percent _ -> Left $ mkServiceErr $ "Could not parse battery state: " <> show infoStr
    Status _ -> Left $ mkServiceErr $ "Could not parse battery percent: " <> show infoStr
  where
    mkServiceErr = MkServiceErr "Battery" "Parse error" . T.pack

toNote :: Map Int Note -> BatteryState -> Maybe Note
toNote lvlNoteMap (MkBatteryState currLvl status) = case status of
  Discharging ->
    let violatedLvl = Sorted.leastUpperBound currLvl alertLvls
     in -- TODO: Dependent Map?
        (=<<) (`Map.lookup` lvlNoteMap) violatedLvl
  _ -> Nothing
  where
    alertLvls = Sorted.unsafeMkSorted $ Map.keys lvlNoteMap

batteryNNote :: Int -> Maybe Icon -> UrgencyLevel -> Timeout -> Note
batteryNNote n icon urgency = Event.mkNote icon summary body hints
  where
    summary = "Battery"
    body = Just (Text ("Power is less than " <> show n <> "%"))
    hints = [Urgency urgency]

data BatteryState = MkBatteryState
  { level :: Int,
    status :: BatteryStatus
  }
  deriving (Eq, Show)

data BatteryResult
  = None
  | Percent Int
  | Status BatteryStatus
  | Both BatteryState
  deriving (Show)

instance Semigroup BatteryResult where
  Both s <> _ = Both s
  _ <> Both s = Both s
  None <> r = r
  l <> None = l
  Percent n <> Status s = Both $ MkBatteryState n s
  Status s <> Percent n = Both $ MkBatteryState n s
  l <> _ = l

instance Monoid BatteryResult where
  mempty = None

parseBattery :: Text -> BatteryResult
parseBattery txt = foldMap parseLine ts
  where
    ts = T.lines txt

parseLine :: Text -> BatteryResult
parseLine ln = case AP.parseOnly parseState ln of
  Right s -> Status s
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

data BatteryStatus
  = Charging
  | Discharging
  | Full
  deriving (Eq, Show)

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