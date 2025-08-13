{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'NaviNote' type, representing notifications.
module Navi.Data.NaviNote
  ( NaviNote (..),
    urgencyLevelOptDecoder,
    Timeout (..),
    timeoutOptDecoder,
    replaceOut,
    CustomResult (..),
    parseCustomResult,
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

replaceOut :: Text -> NaviNote -> NaviNote
replaceOut outVal = over' (#body % _Just) (T.replace "$out" outVal)

-- | Custom text result for "single" and "multiple" services.
data CustomResult
  = -- | Output was arbitrary text.
    CustomText Text
  | -- | Outout was (result, out) for custom output.
    CustomOut (Text, Text)
  deriving stock (Show)

instance Eq CustomResult where
  x == y = toResult x == toResult y

instance Ord CustomResult where
  x <= y = toResult x <= toResult y

toResult :: CustomResult -> Text
toResult (CustomText r) = r
toResult (CustomOut (r, _)) = r

parseCustomResult :: Text -> CustomResult
parseCustomResult txt = mToE $ do
  r1 <- T.stripPrefix "(" txt
  let (result, r2) = T.break (== ',') r1
  (',', r3) <- T.uncons r2
  let (out, r4) = T.break (== ')') r3
  (')', _) <- T.uncons r4
  pure (T.strip result, T.strip out)
  where
    mToE Nothing = CustomText txt
    mToE (Just t) = CustomOut t
