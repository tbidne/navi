{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides 'NaviT', the main type that runs the application.
module Navi.NaviT
  ( NaviT (..),
    runNaviT,
  )
where

import DBus.Notify qualified as DBusN
import Navi.Effects.MonadLoggerContext (MonadLoggerContext (..), addNamespace)
import Navi.Effects.MonadLoggerContext qualified as MonadLoggerContext
import Navi.Effects.MonadMutRef (MonadMutRef (..))
import Navi.Effects.MonadNotify (MonadNotify (..))
import Navi.Effects.MonadQueue (MonadQueue (..))
import Navi.Effects.MonadShell (MonadShell (..))
import Navi.Effects.MonadSystemInfo (MonadSystemInfo (..))
import Navi.Effects.MonadSystemTime (MonadSystemTime (..))
import Navi.Env.Core
  ( HasLogEnv (..),
    HasLogNamespace (..),
    HasLogQueue (getLogQueue),
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
  {-# INLINEABLE query #-}

-- Concrete IO rather than MonadIO so that we can write instances over
-- other MonadIOs (i.e. in tests)
instance MonadNotify (NaviT DBusEnv IO) where
  sendNote naviNote = addNamespace "dbus" $ do
    $(logDebug) (showt note)
    client <- asks getClient
    liftIO $ sendDbus client note
    where
      note = naviToDBus naviNote
      sendDbus c = void . DBusN.notify c
  {-# INLINEABLE sendNote #-}

-- Concrete IO rather than MonadIO so that we can write instances over
-- other MonadIOs (i.e. in tests)
instance MonadNotify (NaviT NotifySendEnv IO) where
  sendNote naviNote = addNamespace "notify-send" $ do
    $(logDebug) noteTxt
    liftIO $ void $ Proc.readCreateProcess cp "notify-send"
    where
      noteTxt = naviToNotifySend naviNote
      cp = Proc.shell $ unpack noteTxt
  {-# INLINEABLE sendNote #-}

-- Concrete IO rather than MonadIO so that we can write instances over
-- other MonadIOs (i.e. in tests)
instance
  ( HasLogEnv env,
    HasLogNamespace env,
    HasLogQueue env
  ) =>
  MonadLogger (NaviT env IO)
  where
  monadLoggerLog loc _src lvl msg = do
    logQueue <- asks getLogQueue
    logLevel <- asks (view #logLevel . getLogEnv)
    when (logLevel <= lvl) $ do
      formatted <- MonadLoggerContext.formatLog True loc lvl msg
      writeQueue logQueue formatted

instance
  ( HasLogEnv env,
    HasLogNamespace env,
    HasLogQueue env
  ) =>
  MonadLoggerContext (NaviT env IO)
  where
  getNamespace = asks getLogNamespace
  localNamespace = local . localLogNamespace

instance MonadSystemTime (NaviT env IO) where
  getSystemTime = lift getSystemTime
  getSystemZonedTime = lift getSystemZonedTime

instance MonadMutRef ref m => MonadMutRef ref (NaviT e m) where
  newRef = lift . newRef
  {-# INLINEABLE newRef #-}
  readRef = lift . readRef
  {-# INLINEABLE readRef #-}
  writeRef ref = lift . writeRef ref
  {-# INLINEABLE writeRef #-}

-- | Runs 'NaviT'.
runNaviT :: NaviT env m a -> env -> m a
runNaviT (MkNaviT rdr) = runReaderT rdr
{-# INLINEABLE runNaviT #-}
