module Navi.Services.Battery.Level.Event
  ( mkBatteryEvent,
  )
where

import Data.Attoparsec.Combinator qualified as AP
import Data.Attoparsec.Text (Parser)
import Data.Attoparsec.Text qualified as AP
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Navi.Data.BoundedN (BoundedN (..))
import Navi.Data.BoundedN qualified as BoundedN
import Navi.Data.NaviNote (NaviNote (..))
import Navi.Data.Sorted (Sorted)
import Navi.Data.Sorted qualified as Sorted
import Navi.Event.Types
  ( Command (..),
    ErrorNote (..),
    Event (..),
    EventErr,
    RepeatEvent (..),
  )
import Navi.Event.Types qualified as ETypes
import Navi.Prelude
import Navi.Services.Battery.Types
  ( BatteryLevel,
    BatteryState (..),
    BatteryStatus (..),
    BatteryType (..),
  )

mkBatteryEvent ::
  [(BatteryLevel, NaviNote)] ->
  BatteryType ->
  RepeatEvent ref BatteryState ->
  ErrorNote ref ->
  Event ref BatteryState
mkBatteryEvent lvlNoteList batteryType re en =
  MkEvent
    { name = "Battery",
      command = cmd,
      parser = parserFn,
      raiseAlert = toNote lvlNoteMap,
      repeatEvent = re,
      errorNote = en
    }
  where
    lvlNoteMap = Map.fromList lvlNoteList
    upperBoundMap = initBoundMap $ Sorted.fromList $ fmap fst lvlNoteList
    cmd = typeToCmd batteryType
    parserFn = queryFn upperBoundMap

queryFn :: Map BatteryLevel BatteryLevel -> Text -> Either EventErr BatteryState
queryFn upperBoundMap infoStr = do
  case parseBattery infoStr of
    Both MkBatteryState {level, status} ->
      case Map.lookup level upperBoundMap of
        Nothing ->
          Left $
            ETypes.MkEventErr
              "Battery"
              "Bound error"
              $ "Could not find bound for: " <> T.pack (show level)
        Just bound -> Right $ MkBatteryState bound status
    None -> Left $ mkServiceErr $ "Could not parse battery percent and state: " <> show infoStr
    Percent _ -> Left $ mkServiceErr $ "Could not parse battery state: " <> show infoStr
    Status _ -> Left $ mkServiceErr $ "Could not parse battery percent: " <> show infoStr
  where
    mkServiceErr = ETypes.MkEventErr "Battery" "Parse error" . T.pack

toNote :: Map BatteryLevel NaviNote -> BatteryState -> Maybe NaviNote
toNote lvlNoteMap MkBatteryState {level, status} = case status of
  Discharging -> Map.lookup level lvlNoteMap
  _ -> Nothing

initBoundMap :: Sorted BatteryLevel -> Map BatteryLevel BatteryLevel
initBoundMap = Map.fromList . go 0 . Sorted.toList
  where
    go prev [] = [(unsafeB i, unsafeB 100) | i <- [prev .. 100]]
    go prev (n@(MkBoundedN x) : xs) =
      [(unsafeB i, n) | i <- [prev .. x]] <> go x xs
    unsafeB = BoundedN.unsafeBoundedN

data BatteryResult
  = None
  | Percent BatteryLevel
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

parsePercent :: Parser BatteryLevel
parsePercent =
  AP.skipSpace
    *> AP.string "percentage:"
    *> AP.skipSpace
    *> parseNN
    <* end
  where
    parseNN = AP.decimal >>= maybe empty pure . BoundedN.mkBoundedN
    end = AP.char '%' *> AP.skipSpace

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
