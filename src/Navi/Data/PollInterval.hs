{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'PollInterval' type.
--
-- @since 0.1
module Navi.Data.PollInterval
  ( PollInterval (..),
    pollIntervalCodec,
    toSleepTime,
  )
where

import Data.Text qualified as T
import Data.Time.Relative qualified as Rel
import Navi.Prelude
import Toml (TomlCodec)
import Toml qualified

-- | Represents how often to poll for service changes, in seconds.
--
-- @since 0.1
newtype PollInterval = MkPollInterval {unPollInterval :: Natural}
  deriving stock (Eq, Ord, Show)

-- | @since 0.1
makeFieldLabelsNoPrefix ''PollInterval

-- | @since 0.1
instance Bounded PollInterval where
  minBound = MkPollInterval 0
  maxBound = maxPollInterval

-- | Codec for 'PollInterval'.
--
-- @since 0.1
pollIntervalCodec :: TomlCodec PollInterval
pollIntervalCodec =
  Toml.textBy showPollInterval parsePollInterval "poll-interval"
  where
    showPollInterval :: PollInterval -> Text
    showPollInterval (MkPollInterval x) = T.pack $ show x
    parsePollInterval :: Text -> Either Text PollInterval
    parsePollInterval t = case Rel.fromString (T.unpack t) of
      Left err -> Left $ "Could not parse poll-interval: " <> T.pack err
      Right relTime ->
        let relTimeSec = Rel.toSeconds relTime
         in if MkPollInterval relTimeSec > maxBound
              then Left $ T.pack $ "Poll interval too large: " <> show relTimeSec <> ". Maximum seconds is " <> show (maxBound :: PollInterval)
              else Right $ MkPollInterval relTimeSec

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
