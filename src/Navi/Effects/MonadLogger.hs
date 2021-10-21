module Navi.Effects.MonadLogger
  ( MonadLogger (..),
  )
where

import Katip (LogStr, Namespace, Severity)
import Navi.Prelude

class Monad m => MonadLogger m where
  logFm :: Severity -> LogStr -> m ()
  logText :: Severity -> Text -> m ()
  addNamespace :: Namespace -> m a -> m a
