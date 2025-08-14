{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides 'NaviT', the main type that runs the application.
module Navi.NaviT
  ( NaviT (..),
    runNaviT,
  )
where

import Effects.Logger.Namespace
  ( defaultLogFormatter,
    formatLog,
  )
import Effects.Process.Typed qualified as TP
import Effects.Time (MonadTime)
import Navi.Config.Types (NoteSystem (AppleScript, DBus, NotifySend))
import Navi.Effects.MonadNotify (MonadNotify (sendNote))
import Navi.Effects.MonadSystemInfo (MonadSystemInfo)
import Navi.Env.AppleScript (naviToAppleScript)
import Navi.Env.Core
  ( Env,
    HasLogEnv (getLogEnv),
  )
import Navi.Env.DBus (MonadDBus)
import Navi.Env.DBus qualified as DBus
import Navi.Env.NotifySend (naviToNotifySend)
import Navi.Prelude

-- | NaviT is the core type used to run the application.
type NaviT :: Type -> (Type -> Type) -> Type -> Type
newtype NaviT e m a = MkNaviT (ReaderT e m a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadAsync,
      MonadCatch,
      MonadDBus,
      MonadFileReader,
      MonadHandleWriter,
      MonadIO,
      MonadIORef,
      MonadMask,
      MonadReader e,
      MonadSTM,
      MonadSystemInfo,
      MonadTerminal,
      MonadTime,
      MonadThread,
      MonadThrow,
      MonadTypedProcess
    )
    via (ReaderT e m)

instance
  ( MonadDBus m,
    MonadSTM m,
    MonadTime m,
    MonadThread m,
    MonadTypedProcess m
  ) =>
  MonadNotify (NaviT Env m)
  where
  sendNote naviNote =
    asks (view #notifySystem) >>= \case
      AppleScript -> addNamespace "apple-script" $ do
        let noteTxt = naviToAppleScript naviNote
        $(logDebug) noteTxt
        void $ TP.readProcess (mkProc noteTxt)
      DBus client -> addNamespace "dbus" $ do
        $(logDebug) (showt naviNote)
        void $ DBus.notify client naviNote
      NotifySend -> addNamespace "notify-send" $ do
        let noteTxt = naviToNotifySend naviNote
        $(logDebug) noteTxt
        void $ TP.readProcess (mkProc noteTxt)
    where
      mkProc = TP.shell . unpackText

instance
  ( MonadSTM m,
    MonadTime m,
    MonadThread m
  ) =>
  MonadLogger (NaviT Env m)
  where
  monadLoggerLog loc _src lvl msg = do
    mLogEnv <- asks getLogEnv
    case mLogEnv of
      Just logEnv -> do
        let logQueue = logEnv ^. #logQueue
            logLevel = logEnv ^. #logLevel
        when (logLevel <= lvl) $ do
          formatted <- formatLog formatter lvl msg
          writeTBQueueA logQueue formatted
      Nothing -> pure ()
    where
      formatter = set' #threadLabel True (defaultLogFormatter loc)

-- | Runs 'NaviT'.
runNaviT :: NaviT env m a -> env -> m a
runNaviT (MkNaviT rdr) = runReaderT rdr
{-# INLINEABLE runNaviT #-}
