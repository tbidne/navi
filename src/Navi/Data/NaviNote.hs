-- | Provides the 'NaviNote' type, representing notifications.
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

-- | 'NaviNote' represents desktop notifications.
data NaviNote = MkNaviNote
  { -- | Text summary.
    summary :: Text,
    -- | Text body.
    body :: Maybe Text,
    -- | Notification image.
    image :: Maybe Icon,
    -- | Urgency (e.g. low, critical)
    urgency :: Maybe UrgencyLevel,
    -- | Determines how long the notification stays on-screen.
    timeout :: Maybe Timeout
  }
  deriving (Generic, Show)

-- | Determines how long a notification persists.
data Timeout
  = Never
  | Seconds NonNegative
  deriving (Generic, Show)

-- | Codec for 'NaviNote'.
naviNoteCodec :: TomlCodec NaviNote
naviNoteCodec =
  MkNaviNote
    <$> summaryCodec .= summary
    <*> Toml.dioptional bodyCodec .= body
    <*> Toml.dioptional appImageCodec .= image
    <*> Toml.dioptional urgencyLevelCodec .= urgency
    <*> Toml.dioptional timeoutCodec .= timeout

-- | Codec for the 'NaviNote' 'summary'.
summaryCodec :: TomlCodec Text
summaryCodec = Toml.text "summary"

-- | Codec for the 'NaviNote' 'body'.
bodyCodec :: TomlCodec Text
bodyCodec = Toml.text "body"

-- | Codec for the 'NaviNote' 'image'.
appImageCodec :: TomlCodec Icon
appImageCodec = appImageKeyCodec "app-image"

-- | Codec for the 'NaviNote' 'image' with a custom 'Key'.
appImageKeyCodec :: Key -> TomlCodec Icon
appImageKeyCodec key = appImagePathCodec <|> appImageIconCodec
  where
    appImageIconCodec = Toml.dimatch matchIcon Icon $ Toml.string $ key <> "-path"
    matchIcon (Icon i) = Just i
    matchIcon _ = Nothing
    appImagePathCodec = Toml.dimatch matchFile File $ Toml.string $ key <> "-file"
    matchFile (File p) = Just p
    matchFile _ = Nothing

-- | Codec for the 'NaviNote' 'urgency'.
urgencyLevelCodec :: TomlCodec UrgencyLevel
urgencyLevelCodec = urgencyLevelKeyCodec "urgency"

-- | Codec for the 'NaviNote' 'urgency' with custom 'Key'.
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

-- | Codec for the 'NaviNote' 'timeout'.
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
