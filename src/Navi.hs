-- | This module provides the core application type and logic.
module Navi
  ( NaviT (..),
    runNavi,
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Control.Monad.Trans (MonadTrans (..))
import DBus.Notify (Client, UrgencyLevel (..))
import Data.Text.Lazy.Builder qualified as T
import Data.Void (Void)
import Katip (Katip (..), KatipContext (..), LogStr (..), Severity (..))
import Katip qualified as K
import Navi.Data.NaviNote (NaviNote (..))
import Navi.Effects
  ( MonadLogger (..),
    MonadMutRef (..),
    MonadNotify (..),
    MonadShell (..),
  )
import Navi.Env (Env (..))
import Navi.Event (AnyEvent (..), Event (..), EventErr (..))
import Navi.Event qualified as Event
import Navi.Prelude
import Optics.Generic (GField (..))
import Optics.Operators ((^.))

-- | NaviT is the core type used to run the application.
type NaviT :: Type -> (Type -> Type) -> Type -> Type
newtype NaviT e m a = MkNaviT {runNaviT :: ReaderT e m a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadNotify,
      MonadShell,
      MonadReader e
    )
    via (ReaderT e m)
  deriving (MonadTrans) via (ReaderT e)

instance MonadIO m => Katip (NaviT (Env ref) m) where
  getLogEnv = asks logEnv
  localLogEnv = updateEnvField logEnv (\x e -> e {logEnv = x})

instance MonadIO m => KatipContext (NaviT (Env ref) m) where
  getKatipContext = asks logCtx
  localKatipContext = updateEnvField logCtx (\x e -> e {logCtx = x})
  getKatipNamespace = asks logNamespace
  localKatipNamespace = updateEnvField logNamespace (\x e -> e {logNamespace = x})

updateEnvField :: MonadReader e m => (e -> f1) -> (f2 -> e -> e) -> (f1 -> f2) -> m a -> m a
updateEnvField getter setter modifier m = do
  f <- asks getter
  local (setter (modifier f)) m

instance MonadIO m => MonadLogger (NaviT (Env ref) m) where
  logFm = K.logFM
  logText s = logFm s . LogStr . T.fromText
  addNamespace = K.katipAddNamespace

instance MonadMutRef m ref => MonadMutRef (NaviT e m) ref where
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref = lift . writeRef ref

-- | Entry point for the application.
runNavi ::
  ( MonadLogger m,
    MonadMutRef m ref,
    MonadNotify m,
    MonadShell m,
    MonadReader (Env ref) m
  ) =>
  m Void
runNavi = do
  pollInterval <- asks pollInterval
  events <- asks events
  client <- asks client
  forever $ do
    sleep pollInterval
    logText DebugS "Checking alerts..."
    traverse (processEvent client) events

processEvent ::
  ( MonadLogger m,
    MonadMutRef m ref,
    MonadNotify m,
    MonadShell m
  ) =>
  Client ->
  AnyEvent ref ->
  m ()
processEvent client (MkAnyEvent event) = Event.runEvent event >>= handleResult
  where
    name = event ^. gfield @"name"
    repeatEvent = event ^. gfield @"repeatEvent"
    errorNote = event ^. gfield @"errorNote"
    raiseAlert = event ^. gfield @"raiseAlert"

    handleResult (Left err@MkEventErr {short, long}) = addNamespace "Handling Result" $ do
      blockErrEvent <- Event.blockErr errorNote
      logText ErrorS $ mkLog "Event Error" $ short <> ": " <> long
      if blockErrEvent
        then logText DebugS "Error note blocked"
        else sendNote client (serviceErrToNote err)
    handleResult (Right result) = addNamespace "Handling Result" $ do
      case raiseAlert result of
        Nothing -> do
          logText DebugS $ mkLog "No alert to raise" result
          Event.updatePrevTrigger repeatEvent result
        Just note -> do
          blocked <- Event.blockRepeat repeatEvent result
          if blocked
            then logText DebugS $ mkLog "Alert blocked" result
            else do
              logText InfoS $ mkLog "Sending alert" result
              Event.updatePrevTrigger repeatEvent result
              sendNote client note
    mkLog :: Show a => Text -> a -> Text
    mkLog msg x = "[" <> name <> "] " <> msg <> ": " <> showt x

serviceErrToNote :: EventErr -> NaviNote
serviceErrToNote eventErr =
  MkNaviNote
    { summary = "Event Error",
      body = Just $ name <> ": " <> short,
      image = Nothing,
      urgency = Just Critical,
      timeout = Nothing
    }
  where
    name = eventErr ^. gfield @"name"
    short = eventErr ^. gfield @"short"
