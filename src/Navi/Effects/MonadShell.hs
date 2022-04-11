-- | Provides a \"shell\" effect.
module Navi.Effects.MonadShell
  ( MonadShell (..),
  )
where

import Control.Concurrent qualified as CC
import Navi.Prelude

-- | This class represents effects that a shell can provide.
class Monad m => MonadShell m where
  readFile :: FilePath -> m Text
  sleep :: Word16 -> m ()

instance MonadShell IO where
  readFile = readFileUtf8Lenient
  sleep = CC.threadDelay . (*) 1_000_000 . w16ToInt

instance MonadShell m => MonadShell (ReaderT e m) where
  readFile = lift . readFile
  sleep = lift . sleep
