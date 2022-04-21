-- | Provides a logger effect.
module Navi.Effects.MonadLogger
  ( MonadLogger (..),
    sendLogQueue,
  )
where

import Katip (Namespace)
import Navi.Data.NaviLog (NaviLog)
import Navi.Effects.MonadQueue (MonadQueue (..))
import Navi.Env.Core (HasLogQueue (..))
import Navi.Prelude

-- | 'MonadLogger' allows us to use 'Katip.Katip' with a pure interface
-- ('Katip.Katip' has a 'Control.Monad.IO.Class.MonadIO' constraint).
class Monad m => MonadLogger m where
  logText :: NaviLog -> m ()
  addNamespace :: Namespace -> m a -> m a

-- | Convenience function for retrieving a 'Navi.Data.NaviQueue.NaviQueue'
-- 'NaviLog' from the @env@ and sending the log.
sendLogQueue ::
  (HasLogQueue env, MonadQueue m, MonadReader env m) =>
  NaviLog ->
  m ()
sendLogQueue lg =
  asks getLogQueue >>= (`writeQueue` lg)
