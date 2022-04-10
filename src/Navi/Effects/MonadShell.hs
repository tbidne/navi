-- | Provides a \"shell\" effect.
module Navi.Effects.MonadShell
  ( MonadShell (..),
  )
where

import Control.Concurrent qualified as CC
import Navi.Prelude
import Numeric.Data.NonNegative (NonNegative)
import Numeric.Data.NonNegative qualified as NonNegative

-- | This class represents effects that a shell can provide.
class Monad m => MonadShell m where
  readFile :: FilePath -> m Text
  sleep :: NonNegative Int -> m ()

instance MonadShell IO where
  readFile = readFileUtf8Lenient
  sleep = CC.threadDelay . (*) 1_000_000 . NonNegative.unNonNegative

instance MonadShell m => MonadShell (ReaderT e m) where
  readFile = lift . readFile
  sleep = lift . sleep
