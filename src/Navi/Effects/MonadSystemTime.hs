-- | Provides the 'MonadSystemTime' class.
--
-- @since 0.1
module Navi.Effects.MonadSystemTime
  ( MonadSystemTime (..),
    formatSystemTime,
    formatSystemZonedTime,
  )
where

import Data.Time.Format qualified as Format
import Data.Time.LocalTime qualified as Local
import Data.Time.LocalTime.Compat (LocalTime, ZonedTime)
import Navi.Prelude

-- | Class for retrieving the current system time.
--
-- @since 0.1
class Monad m => MonadSystemTime m where
  -- | @since 0.1
  getSystemTime :: HasCallStack => m LocalTime

  -- | @since 0.1
  getSystemZonedTime :: HasCallStack => m ZonedTime

-- | @since 0.1
instance MonadSystemTime IO where
  getSystemTime = Local.zonedTimeToLocalTime <$> Local.getZonedTime
  getSystemZonedTime = Local.getZonedTime

-- | @since 0.1
instance MonadSystemTime m => MonadSystemTime (ReaderT e m) where
  getSystemTime = lift getSystemTime
  getSystemZonedTime = lift getSystemZonedTime

-- | @since 0.1
formatSystemZonedTime :: ZonedTime -> String
formatSystemZonedTime = Format.formatTime Format.defaultTimeLocale format
  where
    format = "%Y-%m-%d %H:%M:%S %Z"

-- | @since 0.1
formatSystemTime :: LocalTime -> String
formatSystemTime = Format.formatTime Format.defaultTimeLocale format
  where
    format = "%Y-%m-%d %H:%M:%S"
