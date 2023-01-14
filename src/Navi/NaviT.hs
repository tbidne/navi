{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides 'NaviT', the main type that runs the application.
module Navi.NaviT
  ( NaviT (..),
    runNaviT,
  )
where

import DBus.Notify qualified as DBusN
import Effects.MonadLoggerNamespace
  ( MonadLoggerNamespace (..),
    addNamespace,
    defaultLogFormatter,
    formatLog,
  )
import Effects.MonadTime (MonadTime (..))
import Navi.Effects.MonadNotify (MonadNotify (..))
import Navi.Effects.MonadSystemInfo (MonadSystemInfo (..))
import Navi.Env.Core
  ( HasLogEnv (..),
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
      MonadAsync,
      MonadCallStack,
      MonadCatch,
      MonadFileReader,
      MonadHandleWriter,
      MonadIO,
      MonadIORef,
      MonadReader e,
      MonadSTM,
      MonadTerminal,
      MonadThread,
      MonadThrow,
      MonadTime
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
    HasLogQueue env
  ) =>
  MonadLogger (NaviT env IO)
  where
  monadLoggerLog loc _src lvl msg = do
    logQueue <- asks getLogQueue
    logLevel <- asks (view #logLevel . getLogEnv)
    when (logLevel <= lvl) $ do
      formatted <- formatLog (defaultLogFormatter loc) lvl msg
      writeTBQueueM logQueue formatted

instance
  ( HasLogEnv env,
    HasLogQueue env
  ) =>
  MonadLoggerNamespace (NaviT env IO)
  where
  getNamespace = asks (view #logNamespace . getLogEnv)
  localNamespace f = local (localLogEnv (over' #logNamespace f))

-- | Runs 'NaviT'.
runNaviT :: NaviT env m a -> env -> m a
runNaviT (MkNaviT rdr) = runReaderT rdr
{-# INLINEABLE runNaviT #-}
