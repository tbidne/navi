module Navi.Data.NaviNote
  ( NaviNote (..),
    Timeout (..),
    naviNoteCodec,
    summaryCodec,
    bodyCodec,
    appImageCodec,
    appImageKeyCodec,
    urgencyLevelCodec,
    urgencyLevelKeyCodec,
    timeoutCodec,
  )
where

import DBus.Notify (Icon (..), UrgencyLevel (..))
import Data.Text qualified as T
import Navi.Data.NonNegative (NonNegative)
import Navi.Data.NonNegative qualified as NN
import Navi.Prelude
import Toml (Key, TomlCodec, (.=))
import Toml qualified

data NaviNote = MkNaviNote
  { summary :: Text,
    body :: Maybe Text,
    image :: Maybe Icon,
    urgency :: Maybe UrgencyLevel,
    timeout :: Maybe Timeout
  }
  deriving (Generic, Show)

data Timeout
  = Never
  | Seconds NonNegative
  deriving (Generic, Show)

naviNoteCodec :: TomlCodec NaviNote
naviNoteCodec =
  MkNaviNote
    <$> summaryCodec .= summary
    <*> Toml.dioptional bodyCodec .= body
    <*> Toml.dioptional appImageCodec .= image
    <*> Toml.dioptional urgencyLevelCodec .= urgency
    <*> Toml.dioptional timeoutCodec .= timeout

summaryCodec :: TomlCodec Text
summaryCodec = Toml.text "summary"

bodyCodec :: TomlCodec Text
bodyCodec = Toml.text "body"

appImageCodec :: TomlCodec Icon
appImageCodec = appImageKeyCodec "app-image"

appImageKeyCodec :: Key -> TomlCodec Icon
appImageKeyCodec key = appImagePathCodec <|> appImageIconCodec
  where
    appImageIconCodec = Toml.dimatch matchIcon Icon $ Toml.string $ key <> "-path"
    matchIcon (Icon i) = Just i
    matchIcon _ = Nothing
    appImagePathCodec = Toml.dimatch matchFile File $ Toml.string $ key <> "-file"
    matchFile (File p) = Just p
    matchFile _ = Nothing

urgencyLevelCodec :: TomlCodec UrgencyLevel
urgencyLevelCodec = urgencyLevelKeyCodec "urgency"

urgencyLevelKeyCodec :: Key -> TomlCodec UrgencyLevel
urgencyLevelKeyCodec = Toml.textBy showUrgencyLevel parseUrgencyLevel
  where
    showUrgencyLevel Low = "low"
    showUrgencyLevel Normal = "normal"
    showUrgencyLevel Critical = "critical"
    parseUrgencyLevel "low" = Right Low
    parseUrgencyLevel "normal" = Right Normal
    parseUrgencyLevel "critical" = Right Critical
    parseUrgencyLevel other = Left $ "Unsupported urgency level: " <> other

timeoutCodec :: TomlCodec Timeout
timeoutCodec =
  Toml.textBy showTimeout parseTimeout "timeout"
  where
    showTimeout Never = "never"
    showTimeout (Seconds s) = T.pack $ show s
    parseTimeout "never" = Right Never
    parseTimeout other =
      case NN.readNonNegative (T.unpack other) of
        Just s -> Right $ Seconds s
        Nothing -> Left $ "Unsupported timeout: " <> other
