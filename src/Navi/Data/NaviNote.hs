{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'NaviNote' type, representing notifications.
module Navi.Data.NaviNote
  ( NaviNote (..),
    Timeout (..),
    naviNoteCodec,
    summaryCodec,
    bodyCodec,
    urgencyLevelCodec,
    urgencyLevelKeyCodec,
    timeoutCodec,
  )
where

import DBus.Notify (UrgencyLevel (..))
import Data.Text qualified as T
import Navi.Prelude
import Optics.TH qualified as O
import Refined (NonNegative, Refined)
import Refined qualified as R
import Text.Read qualified as TR
import Toml (Key, TomlCodec, (.=))
import Toml qualified

-- | Determines how long a notification persists.
data Timeout
  = Never
  | Seconds (Refined NonNegative Int)
  deriving (Show)

O.makeFieldLabelsNoPrefix ''Timeout

-- | 'NaviNote' represents desktop notifications.
data NaviNote = MkNaviNote
  { -- | Text summary.
    summary :: Text,
    -- | Text body.
    body :: Maybe Text,
    -- | Urgency (e.g. low, critical)
    urgency :: Maybe UrgencyLevel,
    -- | Determines how long the notification stays on-screen.
    timeout :: Maybe Timeout
  }
  deriving (Show)

O.makeFieldLabelsNoPrefix ''NaviNote

-- | Codec for 'NaviNote'.
naviNoteCodec :: TomlCodec NaviNote
naviNoteCodec =
  MkNaviNote
    <$> summaryCodec .= summary
    <*> Toml.dioptional bodyCodec .= body
    <*> Toml.dioptional urgencyLevelCodec .= urgency
    <*> Toml.dioptional timeoutCodec .= timeout

-- | Codec for the 'NaviNote' 'summary'.
summaryCodec :: TomlCodec Text
summaryCodec = Toml.text "summary"

-- | Codec for the 'NaviNote' 'body'.
bodyCodec :: TomlCodec Text
bodyCodec = Toml.text "body"

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
      case readNN (T.unpack other) of
        Right s -> Right $ Seconds s
        Left _ -> Left $ "Unsupported timeout: " <> other
    readNN = TR.readEither >=> first show . R.refine
