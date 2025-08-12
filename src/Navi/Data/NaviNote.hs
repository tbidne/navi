{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'NaviNote' type, representing notifications.
module Navi.Data.NaviNote
  ( NaviNote (..),
    urgencyLevelOptDecoder,
    Timeout (..),
    timeoutOptDecoder,
    replaceTrigger,
  )
where

import DBus.Notify (UrgencyLevel)
import Data.Bits (toIntegralSized)
import Data.Text qualified as T
import Navi.Prelude
import Navi.Utils (urgencyLevelOptDecoder)

-- | Determines how long a notification persists.
--
-- @since 0.1
data Timeout
  = Never
  | Seconds Word16
  deriving stock (Eq, Ord, Show)

-- | @since 0.1
instance DecodeTOML Timeout where
  tomlDecoder = makeDecoder $ \case
    String "never" -> pure Never
    String bad -> invalidValue strErr (String bad)
    Integer i -> case toIntegralSized i of
      Just i' -> pure $ Seconds i'
      Nothing -> invalidValue tooLargeErr (Integer i)
    badTy -> typeMismatch badTy
    where
      tooLargeErr = "Timeout integer too large. Max is: " <> showt maxW16
      strErr = "Unexpected timeout. Only valid string is 'never'."
      maxW16 = maxBound @Word16

-- | TOML decoder for optional 'Timeout' with field name "timeout".
--
-- @since 0.1
timeoutOptDecoder :: Decoder (Maybe Timeout)
timeoutOptDecoder = getFieldOptWith tomlDecoder "timeout"

-- | 'NaviNote' represents desktop notifications.
--
-- @since 0.1
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
  deriving stock (Eq, Ord, Show)

makeFieldLabelsNoPrefix ''NaviNote

-- | @since 0.1
instance DecodeTOML NaviNote where
  tomlDecoder =
    MkNaviNote
      <$> getField "summary"
      <*> getFieldOpt "body"
      <*> urgencyLevelOptDecoder
      <*> timeoutOptDecoder

replaceTrigger :: Text -> NaviNote -> NaviNote
replaceTrigger triggerVal =
  over' (#body % _Just) (T.replace "$trigger" triggerVal)
