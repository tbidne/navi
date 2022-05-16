{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides 'NaviT', the main type that runs the application.
module Navi.NaviT
  ( NaviT (..),
    runNaviT,
  )
where

import DBus.Notify qualified as DBusN
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
      MonadBase b,
      MonadBaseControl b,
      MonadIO,
      MonadQueue,
      MonadReader e,
      MonadShell,
      MonadCatch,
      MonadThrow
    )
    via (ReaderT e m)
  deriving (MonadTrans) via (ReaderT e)

-- Concrete IO rather than MonadIO so that we can write instances over
-- other MonadIOs (i.e. in tests)
instance MonadSystemInfo (NaviT env IO) where
  query = liftBase . query
  {-# INLINEABLE query #-}

-- Concrete IO rather than MonadIO so that we can write instances over
-- other MonadIOs (i.e. in tests)
instance MonadNotify (NaviT DBusEnv IO) where
  sendNote naviNote = addNamespace "dbus" $ do
    sendLogQueue $ MkNaviLog DebugS (showt note)
    client <- asks getClient
    liftBase $ sendDbus client note
    where
      note = naviToDBus naviNote
      sendDbus c = void . DBusN.notify c
  {-# INLINEABLE sendNote #-}

-- Concrete IO rather than MonadIO so that we can write instances over
-- other MonadIOs (i.e. in tests)
instance MonadNotify (NaviT NotifySendEnv IO) where
  sendNote naviNote = addNamespace "notify-send" $ do
    sendLogQueue $ MkNaviLog DebugS noteTxt
    liftBase $ void $ Proc.readCreateProcess cp "notify-send"
    where
      noteTxt = naviToNotifySend naviNote
      cp = Proc.shell $ unpack noteTxt
  {-# INLINEABLE sendNote #-}

instance (HasLogEnv env, MonadIO m) => Katip (NaviT env m) where
  getLogEnv = asks getLogEnv
  {-# INLINEABLE getLogEnv #-}
  localLogEnv = updateEnvField overLogEnv
  {-# INLINEABLE localLogEnv #-}

instance
  ( HasLogContexts env,
    HasLogEnv env,
    HasLogNamespace env,
    MonadIO m
  ) =>
  KatipContext (NaviT env m)
  where
  getKatipContext = asks getLogContexts
  {-# INLINEABLE getKatipContext #-}
  localKatipContext = updateEnvField overLogContexts
  {-# INLINEABLE localKatipContext #-}
  getKatipNamespace = asks getLogNamespace
  {-# INLINEABLE getKatipNamespace #-}
  localKatipNamespace = updateEnvField overLogNamespace
  {-# INLINEABLE localKatipNamespace #-}

updateEnvField ::
  MonadReader env m =>
  ((f1 -> f2) -> env -> env) ->
  (f1 -> f2) ->
  m a ->
  m a
updateEnvField overFn modifier = local (overFn modifier)
{-# INLINEABLE updateEnvField #-}

-- Concrete IO rather than MonadIO so that we can write instances over
-- other MonadIOs (i.e. in tests)
instance
  ( HasLogContexts env,
    HasLogEnv env,
    HasLogNamespace env
  ) =>
  MonadLogger (NaviT env IO)
  where
  logText naviLog ns = addNamespace ns $ do
    let log = LogStr $ T.fromText (naviLog ^. #text)
    $(K.logTM) (naviLog ^. #severity) log
  {-# INLINEABLE logText #-}

  addNamespace = K.katipAddNamespace
  {-# INLINEABLE addNamespace #-}

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
