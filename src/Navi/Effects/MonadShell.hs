-- | Provides a \"shell\" effect.
module Navi.Effects.MonadShell
  ( MonadShell (..),
  )
where

import Control.Concurrent qualified as CC
import Navi.Data.PollInterval (PollInterval)
import Navi.Data.PollInterval qualified as PollInt
import Navi.Prelude

-- | This class represents effects that a shell can provide.
class Monad m => MonadShell m where
  readFile :: FilePath -> m Text
  sleep :: PollInterval -> m ()

instance MonadShell IO where
  readFile = readFileUtf8Lenient
  sleep = CC.threadDelay . PollInt.toSleepTime

instance MonadShell m => MonadShell (ReaderT e m) where
  readFile = lift . readFile
  sleep = lift . sleep
