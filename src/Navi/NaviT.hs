{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides 'NaviT', the main type that runs the application.
module Navi.NaviT
  ( NaviT (..),
    runNaviT,
  )
where

import DBus.Notify qualified as DBusN
import Effects.LoggerNS
  ( MonadLoggerNS (getNamespace, localNamespace),
    addNamespace,
    defaultLogFormatter,
    formatLog,
  )
import Effects.System.Terminal
  ( MonadTerminal
      ( getChar,
        getContents',
        getLine,
        getTerminalSize,
        putBinary,
        putStr,
        supportsPretty
      ),
  )
import Effects.Time (MonadTime (getMonotonicTime, getSystemZonedTime))
import Navi.Effects.MonadNotify (MonadNotify (sendNote))
import Navi.Effects.MonadSystemInfo (MonadSystemInfo (query))
import Navi.Env.Core
  ( HasLogEnv (getLogEnv, localLogEnv),
    HasLogQueue (getLogQueue),
  )
import Navi.Env.DBus (DBusEnv, HasDBusClient (getClient), naviToDBus)
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
      MonadCatch,
      MonadFileReader,
      MonadHandleWriter,
      MonadIO,
      MonadIORef,
      MonadMask,
      MonadReader e,
      MonadSTM,
      MonadThread,
      MonadThrow
    )
    via (ReaderT e m)

-- Manual instances so tests can roll their own
instance MonadTerminal (NaviT env IO) where
  getChar = liftIO getChar
  getLine = liftIO getLine
  getContents' = liftIO getContents'
  getTerminalSize = liftIO getTerminalSize
  putBinary = liftIO . putBinary
  putStr = liftIO . putStr
  putStrLn = liftIO . putStrLn
  supportsPretty = liftIO supportsPretty

instance MonadTime (NaviT env IO) where
  getSystemZonedTime = liftIO getSystemZonedTime
  getMonotonicTime = liftIO getMonotonicTime

-- Concrete IO rather than MonadIO so that we can write instances over
-- other MonadIOs (i.e. in tests)
instance MonadSystemInfo (NaviT env IO) where
  query = liftIO . query

-- Concrete IO rather than MonadIO so that we can write instances over
-- other MonadIOs (i.e. in tests)
instance MonadNotify (NaviT DBusEnv IO) where
  sendNote naviNote = addNamespace "dbus" $ do
    $(logDebug) (showt note)
    client <- asks getClient
    liftIO $ addCS $ sendDbus client note
    where
      note = naviToDBus naviNote
      sendDbus c = void . DBusN.notify c

-- Concrete IO rather than MonadIO so that we can write instances over
-- other MonadIOs (i.e. in tests)
instance MonadNotify (NaviT NotifySendEnv IO) where
  sendNote naviNote = addNamespace "notify-send" $ do
    $(logDebug) noteTxt
    liftIO $ addCS $ void $ Proc.readCreateProcess cp "notify-send"
    where
      noteTxt = naviToNotifySend naviNote
      cp = Proc.shell $ unpack noteTxt

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
      formatted <- formatLog formatter lvl msg
      writeTBQueueA logQueue formatted
    where
      formatter = set' #threadLabel True (defaultLogFormatter loc)

instance
  ( HasLogEnv env,
    HasLogQueue env
  ) =>
  MonadLoggerNS (NaviT env IO)
  where
  getNamespace = asks (view #logNamespace . getLogEnv)
  localNamespace f = local (localLogEnv (over' #logNamespace f))

-- | Runs 'NaviT'.
runNaviT :: NaviT env m a -> env -> m a
runNaviT (MkNaviT rdr) = runReaderT rdr
{-# INLINEABLE runNaviT #-}
