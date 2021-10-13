module Navi.Note.Toml
  ( noteCodec,
    appImageCodec,
    bodyCodec,
    summaryCodec,
    actionsCodec,
    hintsCodec,
    timeoutCodec,
    urgencyLevelCodec,
  )
where

import DBus.Notify
  ( Action,
    Body (..),
    Hint (..),
    Icon (..),
    Note (..),
    Timeout (..),
    UrgencyLevel (..),
  )
import Data.Text qualified as T
import Navi.Prelude
import Text.Read qualified as TR
import Toml (TomlCodec, (.=))
import Toml qualified

noteCodec :: TomlCodec Note
noteCodec =
  Note
    <$> appNameCodec .= appName
    <*> appImageCodec .= appImage
    <*> summaryCodec .= summary
    <*> bodyCodec .= body
    <*> actionsCodec .= actions
    <*> hintsCodec .= hints
    <*> timeoutCodec .= expiry

appNameCodec :: TomlCodec String
appNameCodec = pure "navi"

appImageCodec :: TomlCodec (Maybe Icon)
appImageCodec = Toml.dioptional $ appImagePathCodec <|> appImageIconCodec
  where
    appImageIconCodec = Toml.dimatch matchIcon Icon $ Toml.string "app-image-path"
    matchIcon (Icon i) = Just i
    matchIcon _ = Nothing
    appImagePathCodec = Toml.dimatch matchFile File $ Toml.string "app-image-file"
    matchFile (File p) = Just p
    matchFile _ = Nothing

summaryCodec :: TomlCodec String
summaryCodec = Toml.string "summary"

bodyCodec :: TomlCodec (Maybe Body)
bodyCodec = textCodec
  where
    textCodec = Toml.dioptional $ Toml.dimatch matchText Text $ Toml.string "body-text"
    matchText (Text t) = Just t
    matchText _ = Nothing

actionsCodec :: TomlCodec [(Action, String)]
actionsCodec = pure []

urgencyLevelCodec :: TomlCodec UrgencyLevel
urgencyLevelCodec = Toml.textBy showUrgencyLevel parseUrgencyLevel "urgency-level"
  where
    showUrgencyLevel Low = "low"
    showUrgencyLevel Normal = "normal"
    showUrgencyLevel Critical = "critical"
    parseUrgencyLevel "low" = Right Low
    parseUrgencyLevel "normal" = Right Normal
    parseUrgencyLevel "critical" = Right Critical
    parseUrgencyLevel other = Left $ "Unsupported urgency level: " <> other

hintsCodec :: TomlCodec [Hint]
hintsCodec = Toml.list urgencyCodec "hints"
  where
    urgencyCodec = Toml.dimatch matchUrgency Urgency $ urgencyLevelCodec
    matchUrgency (Urgency u) = Just u
    matchUrgency _ = Nothing

timeoutCodec :: TomlCodec Timeout
timeoutCodec = Toml.textBy showTimeout parseTimeout "timeout"
  where
    showTimeout Never = "never"
    showTimeout Dependent = "dependent"
    showTimeout (Milliseconds m) = T.pack $ show m
    parseTimeout "never" = Right Never
    parseTimeout "dependent" = Right Dependent
    parseTimeout other = case (TR.readMaybe . T.unpack) other of
      Just m -> Right $ Milliseconds m
      Nothing -> Left $ "Unsupported timeout: " <> other
