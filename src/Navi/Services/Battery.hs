module Navi.Services.Battery
  ( mkBatteryEvent,
    batteryNNote,
  )
where

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
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Navi.Data.BoundedN (BoundedN (..))
import Navi.Data.BoundedN qualified as BoundedN
import Navi.Data.Event (Command (..), ErrorEvent (..), Event (..), RepeatEvent (..))
import Navi.Data.Event qualified as Event
import Navi.Data.Sorted (Sorted)
import Navi.Data.Sorted qualified as Sorted
import Navi.Prelude
import Navi.Services.Types (ServiceErr (..))

type BatLevel = BoundedN 0 100

mkBatteryEvent :: [(BatLevel, Note)] -> RepeatEvent BatteryState -> ErrorEvent -> Event
mkBatteryEvent lvlNoteList = Event.mkEvent cmd parserFn lvlNoteMap lookupFn
  where
    lvlNoteMap = Map.fromList lvlNoteList
    upperBoundMap = initBoundMap $ Sorted.fromList $ fmap fst lvlNoteList
    cmd = MkCommand "upower -i `upower -e | grep 'BAT'`"
    parserFn = queryFn upperBoundMap
    lookupFn = toNote

queryFn :: Map BatLevel BatLevel -> Text -> Either ServiceErr BatteryState
queryFn upperBoundMap infoStr = do
  case parseBattery infoStr of
    Both (MkBatteryState l s) ->
      -- TODO: should remove w/ dependent map?
      case Map.lookup l upperBoundMap of
        Nothing ->
          Left $
            MkServiceErr
              "Battery"
              "Bound error"
              $ "Could not find bound for: " <> T.pack (show l)
        Just bound -> Right $ MkBatteryState bound s
    None -> Left $ mkServiceErr $ "Could not parse battery percent and state: " <> show infoStr
    Percent _ -> Left $ mkServiceErr $ "Could not parse battery state: " <> show infoStr
    Status _ -> Left $ mkServiceErr $ "Could not parse battery percent: " <> show infoStr
  where
    mkServiceErr = MkServiceErr "Battery" "Parse error" . T.pack

toNote :: Map BatLevel Note -> BatteryState -> Maybe Note
toNote lvlNoteMap (MkBatteryState currLvl status) = case status of
  Discharging -> Map.lookup currLvl lvlNoteMap
  _ -> Nothing

batteryNNote :: Int -> Maybe Icon -> UrgencyLevel -> Timeout -> Note
batteryNNote n icon urgency = Event.mkNote icon summary body hints
  where
    summary = "Battery"
    body = Just (Text ("Power is less than " <> show n <> "%"))
    hints = [Urgency urgency]

initBoundMap :: Sorted BatLevel -> Map BatLevel BatLevel
initBoundMap = Map.fromList . go 0 . Sorted.toList
  where
    go prev [] = [(unsafeB i, unsafeB 100) | i <- [prev .. 100]]
    go prev (n@(MkBoundedN x) : xs) =
      [(unsafeB i, n) | i <- [prev .. x]] <> go x xs
    unsafeB = BoundedN.unsafeBoundedN

data BatteryState = MkBatteryState
  { level :: BatLevel,
    status :: BatteryStatus
  }
  deriving (Eq, Show)

data BatteryResult
  = None
  | Percent BatLevel
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

parsePercent :: Parser BatLevel
parsePercent =
  AP.skipSpace
    *> AP.string "percentage:"
    *> AP.skipSpace
    *> parseNN
    <* end
  where
    parseNN = AP.decimal >>= maybe empty pure . BoundedN.mkBoundedN
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