{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'PollInterval' type.
--
-- @since 0.1
module Navi.Data.PollInterval
  ( PollInterval (..),
    pollIntervalOptDecoder,
    toSleepTime,
  )
where

import Data.Text qualified as T
import Data.Time.Relative qualified as Rel
import Navi.Prelude

-- | Represents how often to poll for service changes, in seconds.
--
-- @since 0.1
newtype PollInterval = MkPollInterval {unPollInterval :: Natural}
  deriving stock (Eq, Ord, Show)
  deriving (Num) via Natural

-- | @since 0.1
makeFieldLabelsNoPrefix ''PollInterval

-- | @since 0.1
instance Bounded PollInterval where
  minBound = MkPollInterval 0
  maxBound = maxPollInterval

-- | @since 0.1
instance DecodeTOML PollInterval where
  tomlDecoder = makeDecoder $ \case
    String t ->
      case Rel.fromString (unpack t) of
        Left _ -> fail $ unpack $ "Could not parse poll-interval: " <> t
        Right relTime -> ltRelTimeBounds $ Rel.toSeconds relTime
    Integer i -> ltRelTimeBounds (fromIntegral i)
    badTy -> typeMismatch badTy

ltRelTimeBounds :: (MonadFail f) => Natural -> f PollInterval
ltRelTimeBounds n
  | MkPollInterval n <= maxBound = pure $ MkPollInterval n
  | otherwise =
      fail $
        unpack $
          T.concat
            [ "Given poll interval of ",
              showt n,
              " is too large. Maximum seconds is ",
              showt @PollInterval maxBound
            ]

-- | TOML decoder for optional 'PollInterval' with field name 'poll-interval'.
--
-- @since 0.1
pollIntervalOptDecoder :: Decoder (Maybe PollInterval)
pollIntervalOptDecoder = getFieldOptWith tomlDecoder "poll-interval"

-- | Converts a 'PollInterval' into an 'Int' suitable to be used with
-- threadDelay.
--
-- @since 0.1
toSleepTime :: PollInterval -> Int
toSleepTime = fromIntegral . (* 1_000_000) . view #unPollInterval

maxPollInterval :: PollInterval
maxPollInterval = MkPollInterval (fromIntegral mx)
  where
    -- PollInterval represents seconds, and we eventually want to use it in
    -- threadDelay (which requires an Int). This means we have to multiply
    -- by 1_000_000, thus the maximum value we can safely store is
    -- (maxInt / 1_000_000).
    mx = (maxBound :: Int) `div` 1_000_000
