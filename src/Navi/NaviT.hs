-- | Provides 'NaviT', the main type that runs the application.
module Navi.NaviT
  ( NaviT (..),
    runNaviT,
  )
where

import DBus.Notify qualified as DBusN
import Data.Text qualified as T
import Data.Text.Lazy.Builder qualified as T
import Katip (Katip, KatipContext, LogStr (..), Severity (..))
import Katip qualified as K
import Navi.Data.NaviLog (NaviLog (MkNaviLog))
import Navi.Effects.MonadLogger (MonadLogger (..), sendLogQueue)
import Navi.Effects.MonadMutRef (MonadMutRef (..))
import Navi.Effects.MonadNotify (MonadNotify (..))
import Navi.Effects.MonadQueue (MonadQueue (..))
import Navi.Effects.MonadShell (MonadShell (..))
import Navi.Effects.MonadSystemInfo (MonadSystemInfo (..))
import Navi.Env.Core
  ( HasLogContexts (..),
    HasLogEnv (..),
    HasLogNamespace (..),
  )
import Navi.Env.DBus (DBusEnv, HasDBusClient (..), naviToDBus)
import Navi.Env.NotifySend (NotifySendEnv, naviToNotifySend)
import Navi.Prelude
import System.Process qualified as Proc

-- | NaviT is the core type used to run the application.
type NaviT :: Type -> (Type -> Type) -> Type -> Type
newtype NaviT e m a = MkNaviT (ReaderT e m a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadQueue,
      MonadReader e,
      MonadShell,
      MonadUnliftIO
    )
    via (ReaderT e m)
  deriving (MonadTrans) via (ReaderT e)

-- Concrete IO rather than MonadIO so that we can write instances over
-- other MonadIOs (i.e. in tests)
instance MonadSystemInfo (NaviT env IO) where
  query = liftIO . query

-- Concrete IO rather than MonadIO so that we can write instances over
-- other MonadIOs (i.e. in tests)
instance MonadNotify (NaviT DBusEnv IO) where
  sendNote naviNote = do
    sendLogQueue $ MkNaviLog DebugS $ "DBus: " <> pack (show note)
    client <- asks getClient
    liftIO $ sendDbus client note
    where
      note = naviToDBus naviNote
      sendDbus c = void . DBusN.notify c

-- Concrete IO rather than MonadIO so that we can write instances over
-- other MonadIOs (i.e. in tests)
instance MonadNotify (NaviT NotifySendEnv IO) where
  sendNote naviNote = do
    sendLogQueue $ MkNaviLog DebugS $ "NotifySend: " <> noteTxt
    liftIO $ void $ Proc.readCreateProcess cp "notify-send"
    where
      noteTxt = naviToNotifySend naviNote
      cp = Proc.shell $ unpack noteTxt

instance (HasLogEnv env, MonadIO m) => Katip (NaviT env m) where
  getLogEnv = asks getLogEnv
  localLogEnv = updateEnvField overLogEnv

instance
  ( HasLogContexts env,
    HasLogEnv env,
    HasLogNamespace env,
    MonadIO m
  ) =>
  KatipContext (NaviT env m)
  where
  getKatipContext = asks getLogContexts
  localKatipContext = updateEnvField overLogContexts
  getKatipNamespace = asks getLogNamespace
  localKatipNamespace = updateEnvField overLogNamespace

updateEnvField ::
  MonadReader env m =>
  ((f1 -> f2) -> env -> env) ->
  (f1 -> f2) ->
  m a ->
  m a
updateEnvField overFn modifier = local (overFn modifier)

-- Concrete IO rather than MonadIO so that we can write instances over
-- other MonadIOs (i.e. in tests)
instance
  ( HasLogContexts env,
    HasLogEnv env,
    HasLogNamespace env
  ) =>
  MonadLogger (NaviT env IO)
  where
  logText naviLog = do
    let log = LogStr $ T.fromText (naviLog ^. #text)
    K.logFM (naviLog ^. #severity) log

  addNamespace = K.katipAddNamespace

instance MonadMutRef ref m => MonadMutRef ref (NaviT e m) where
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref = lift . writeRef ref

-- | Runs 'NaviT'.
runNaviT :: NaviT env m a -> env -> m a
runNaviT (MkNaviT rdr) = runReaderT rdr
