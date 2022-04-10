-- | This module provides the core application type and logic.
module Navi
  ( NaviT (..),
    runNaviT,
    runNavi,
  )
where

import DBus.Notify (Client, UrgencyLevel (..))
import Data.Text qualified as T
import Data.Text.Lazy.Builder qualified as T
import Katip (Katip, KatipContext, LogStr (..), Severity (..))
import Katip qualified as K
import Navi.Data.NaviNote (NaviNote (..))
import Navi.Effects.MonadLogger (MonadLogger (..))
import Navi.Effects.MonadMutRef (MonadMutRef (..))
import Navi.Effects.MonadNotify (MonadNotify (..))
import Navi.Effects.MonadShell (MonadShell (..))
import Navi.Effects.MonadSystemInfo (MonadSystemInfo (..))
import Navi.Env
  ( HasClient (..),
    HasEvents (..),
    HasLogContexts (..),
    HasLogEnv (..),
    HasLogNamespace (..),
    HasPollInterval (..),
  )
import Navi.Event qualified as Event
import Navi.Event.Types (AnyEvent (..), EventErr (..))
import Navi.Prelude

-- | NaviT is the core type used to run the application.
type NaviT :: Type -> (Type -> Type) -> Type -> Type
newtype NaviT e m a = MkNaviT (ReaderT e m a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadCatch,
      MonadIO,
      MonadNotify,
      MonadShell,
      MonadReader e,
      MonadThrow,
      MonadSystemInfo
    )
    via (ReaderT e m)
  deriving (MonadTrans) via (ReaderT e)

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

instance
  ( HasLogContexts env,
    HasLogEnv env,
    HasLogNamespace env,
    MonadIO m
  ) =>
  MonadLogger (NaviT env m)
  where
  logFm = K.logFM
  logText s = logFm s . LogStr . T.fromText
  addNamespace = K.katipAddNamespace

instance MonadMutRef m ref => MonadMutRef (NaviT e m) ref where
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref = lift . writeRef ref

runNaviT :: NaviT env m a -> env -> m a
runNaviT (MkNaviT rdr) = runReaderT rdr

-- | Entry point for the application.
runNavi ::
  forall ref env m.
  ( HasClient env,
    HasEvents ref env,
    HasPollInterval env,
    MonadCatch m,
    MonadLogger m,
    MonadMutRef m ref,
    MonadNotify m,
    MonadShell m,
    MonadSystemInfo m,
    MonadReader env m
  ) =>
  m Void
runNavi = do
  pollInterval <- asks getPollInterval
  events <- asks (getEvents @ref)
  client <- asks getClient
  forever $ do
    sleep pollInterval
    logText DebugS "Checking alerts..."
    traverse (processEvent client) events

processEvent ::
  forall m ref.
  ( MonadCatch m,
    MonadLogger m,
    MonadMutRef m ref,
    MonadNotify m,
    MonadSystemInfo m
  ) =>
  Client ->
  AnyEvent ref ->
  m ()
processEvent client (MkAnyEvent event) = do
  logText DebugS $ "Checking " <> event ^. #name
  (Event.runEvent event >>= handleSuccess)
    `catch` handleEventErr
    `catch` handleSomeException
  where
    name = event ^. #name
    repeatEvent = event ^. #repeatEvent
    errorNote = event ^. #errorNote
    raiseAlert = event ^. #raiseAlert

    handleSuccess result = addNamespace "Handling Success" $ do
      case raiseAlert result of
        Nothing -> do
          logText DebugS $ mkLog "No alert to raise" result
          Event.updatePrevTrigger repeatEvent result
        Just note -> do
          blocked <- Event.blockRepeat repeatEvent result
          if blocked
            then logText DebugS $ mkLog "Alert blocked" result
            else do
              logText InfoS $ mkLog "Sending note" note
              logText InfoS $ mkLog "Sending alert" result
              Event.updatePrevTrigger repeatEvent result
              sendNote client note

    handleEventErr =
      addNamespace "Handling EventErr"
        . handleErr eventErrToNote

    handleSomeException =
      addNamespace "Handling SomeException"
        . handleErr exToNote

    handleErr :: Exception e => (e -> NaviNote) -> e -> m ()
    handleErr toNote e = do
      blockErrEvent <- Event.blockErr errorNote
      logText ErrorS $ T.pack $ displayException e
      if blockErrEvent
        then logText DebugS "Error note blocked"
        else sendNote client (toNote e)

    mkLog :: Show a => Text -> a -> Text
    mkLog msg x = "[" <> name <> "] " <> msg <> ": " <> showt x

eventErrToNote :: EventErr -> NaviNote
eventErrToNote ex =
  MkNaviNote
    { summary = ex ^. #name,
      body = Just $ ex ^. #short,
      urgency = Just Critical,
      timeout = Nothing
    }

exToNote :: SomeException -> NaviNote
exToNote ex =
  MkNaviNote
    { summary = "Exception",
      body = Just $ T.pack (displayException ex),
      urgency = Just Critical,
      timeout = Nothing
    }
