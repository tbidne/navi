{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'NaviNote' type, representing notifications.
module Navi.Data.NaviNote
  ( NaviNote (..),
    urgencyLevelOptDecoder,
    Timeout (..),
    timeoutOptDecoder,
    replaceOut,
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
  { -- | Text body.
    body :: Maybe Text,
    -- | Text summary.
    summary :: Text,
    -- | Determines how long the notification stays on-screen.
    timeout :: Maybe Timeout,
    -- | Urgency (e.g. low, critical)
    urgency :: Maybe UrgencyLevel
  }
  deriving stock (Eq, Ord, Show)

instance
  (k ~ A_Lens, a ~ Maybe Text, b ~ Maybe Text) =>
  LabelOptic "body" k NaviNote NaviNote a b
  where
  labelOptic =
    lensVL
      $ \f (MkNaviNote a1 a2 a3 a4) ->
        fmap
          (\b -> MkNaviNote b a2 a3 a4)
          (f a1)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Text, b ~ Text) =>
  LabelOptic "summary" k NaviNote NaviNote a b
  where
  labelOptic =
    lensVL
      $ \f (MkNaviNote a1 a2 a3 a4) ->
        fmap
          (\b -> MkNaviNote a1 b a3 a4)
          (f a2)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Maybe Timeout, b ~ Maybe Timeout) =>
  LabelOptic "timeout" k NaviNote NaviNote a b
  where
  labelOptic =
    lensVL
      $ \f (MkNaviNote a1 a2 a3 a4) ->
        fmap
          (\b -> MkNaviNote a1 a2 b a4)
          (f a3)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Maybe UrgencyLevel, b ~ Maybe UrgencyLevel) =>
  LabelOptic "urgency" k NaviNote NaviNote a b
  where
  labelOptic =
    lensVL
      $ \f (MkNaviNote a1 a2 a3 a4) ->
        fmap
          (\b -> MkNaviNote a1 a2 a3 b)
          (f a4)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance DecodeTOML NaviNote where
  tomlDecoder = do
    body <- getFieldOpt "body"
    summary <- getField "summary"
    timeout <- timeoutOptDecoder
    urgency <- urgencyLevelOptDecoder
    pure
      $ MkNaviNote
        { body,
          summary,
          timeout,
          urgency
        }

replaceOut :: Text -> NaviNote -> NaviNote
replaceOut outVal = over' (#body % _Just) (T.replace "$out" outVal)
