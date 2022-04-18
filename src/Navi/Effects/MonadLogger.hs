-- | Provides a logger effect.
module Navi.Effects.MonadLogger
  ( MonadLogger (..),
  )
where

import Katip (LogStr, Namespace, Severity)
import Navi.Prelude

-- | 'MonadLogger' allows us to use 'Katip.Katip' with a pure interface
-- ('Katip.Katip' has a 'Control.Monad.IO.Class.MonadIO' constraint).
class Monad m => MonadLogger m where
  logFm :: Severity -> LogStr -> m ()
  logText :: Severity -> Text -> m ()
  addNamespace :: Namespace -> m a -> m a
