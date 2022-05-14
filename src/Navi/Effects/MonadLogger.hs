-- | Provides a logger effect.
module Navi.Effects.MonadLogger
  ( MonadLogger (..),
    sendLogQueue,
  )
where

import Katip (Namespace)
import Navi.Data.NaviLog (NaviLog)
import Navi.Effects.MonadQueue (MonadQueue (..))
import Navi.Env.Core (HasLogNamespace (..), HasLogQueue (..))
import Navi.Prelude

-- | 'MonadLogger' allows us to use 'Katip.Katip' with a pure interface
-- ('Katip.Katip' has a 'Control.Monad.IO.Class.MonadIO' constraint).
class Monad m => MonadLogger m where
  logText :: NaviLog -> Namespace -> m ()
  addNamespace :: Namespace -> m a -> m a

-- | Convenience function for retrieving a 'Navi.Data.NaviQueue.NaviQueue'
-- 'NaviLog' from the @env@ and sending the log.
sendLogQueue ::
  (HasLogNamespace env, HasLogQueue env, MonadQueue m, MonadReader env m) =>
  NaviLog ->
  m ()
sendLogQueue lg = do
  ns <- asks getLogNamespace
  asks getLogQueue >>= (`writeQueue` (lg, ns))
