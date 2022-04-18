-- | Provides 'NaviT', the main type that runs the application.
module Navi.NaviT
  ( NaviT (..),
    runNaviT,
  )
where

import DBus.Notify (Note)
import DBus.Notify qualified as DBusN
import Data.Text qualified as T
import Data.Text.Lazy.Builder qualified as T
import Katip (Katip, KatipContext, LogStr (..))
import Katip qualified as K
import Navi.Data.NaviNote (NaviNote (..), Timeout (..))
import Navi.Effects.MonadLogger (MonadLogger (..))
import Navi.Effects.MonadMutRef (MonadMutRef (..))
import Navi.Effects.MonadNotify (MonadNotify (..), NotifySystem (..))
import Navi.Effects.MonadShell (MonadShell (..))
import Navi.Effects.MonadSystemInfo (MonadSystemInfo (..))
import Navi.Env.Core
  ( HasLogContexts (..),
    HasLogEnv (..),
    HasLogNamespace (..),
  )
import Navi.Env.DBus (HasDBusClient (..))
import Navi.Prelude

-- | NaviT is the core type used to run the application.
type NaviT :: NotifySystem -> Type -> (Type -> Type) -> Type -> Type
newtype NaviT n e m a = MkNaviT (ReaderT e m a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadCatch,
      MonadIO,
      MonadShell,
      MonadReader e,
      MonadThrow,
      MonadSystemInfo
    )
    via (ReaderT e m)
  deriving (MonadTrans) via (ReaderT e)

instance (HasDBusClient env, MonadIO m) => MonadNotify (NaviT 'DBus env m) where
  sendNote naviNote = do
    client <- asks getClient
    liftIO $ sendDbus client naviNote
    where
      sendDbus c = void . DBusN.notify c . naviToDBus

instance (HasLogEnv env, MonadIO m) => Katip (NaviT n env m) where
  getLogEnv = asks getLogEnv
  localLogEnv = updateEnvField overLogEnv

instance
  ( HasLogContexts env,
    HasLogEnv env,
    HasLogNamespace env,
    MonadIO m
  ) =>
  KatipContext (NaviT n env m)
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

instance
  ( HasLogContexts env,
    HasLogEnv env,
    HasLogNamespace env,
    MonadIO m
  ) =>
  MonadLogger (NaviT n env m)
  where
  logFm = K.logFM
  logText s = logFm s . LogStr . T.fromText
  addNamespace = K.katipAddNamespace

instance MonadMutRef ref m => MonadMutRef ref (NaviT n e m) where
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref = lift . writeRef ref

-- | Runs 'NaviT'.
runNaviT :: NaviT n env m a -> env -> m a
runNaviT (MkNaviT rdr) = runReaderT rdr

naviToDBus :: NaviNote -> Note
naviToDBus naviNote =
  DBusN.Note
    { DBusN.appName = "Navi",
      DBusN.summary = T.unpack $ naviNote ^. #summary,
      DBusN.body = body,
      DBusN.appImage = Nothing,
      DBusN.hints = hints,
      DBusN.expiry = timeout,
      DBusN.actions = []
    }
  where
    body = DBusN.Text . T.unpack <$> naviNote ^. #body
    hints = maybeToList $ DBusN.Urgency <$> naviNote ^. #urgency
    timeout = maybe defTimeout naviToDBusTimeout $ naviNote ^. #timeout
    defTimeout = DBusN.Milliseconds 10_000

naviToDBusTimeout :: Timeout -> DBusN.Timeout
naviToDBusTimeout Never = DBusN.Never
naviToDBusTimeout (Seconds s) = DBusN.Milliseconds $ (* 1_000) $ w16ToInt32 s
  where
    w16ToInt32 :: Word16 -> Int32
    w16ToInt32 = fromIntegral
