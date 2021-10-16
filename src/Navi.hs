module Navi
  ( NaviT (..),
    runNavi,
  )
where

import Control.Exception (Exception (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans (MonadTrans (..))
import DBus.Notify (Body (..), Client, Hint (..), Note, Timeout (..), UrgencyLevel (..))
import Data.Functor.Identity (Identity (..))
import Data.Text qualified as T
import Data.Void (Void)
import Navi.Args (Args (..))
import Navi.Config (Config (..), readConfig)
import Navi.Effects (MonadMutRef (..), MonadNotify (..), MonadShell (..))
import Navi.Event
  ( Event (..),
    EventErr (..),
    EventResult (..),
  )
import Navi.Event qualified as Event
import Navi.Prelude

type NaviT :: (Type -> Type) -> Type -> Type
newtype NaviT m a = MkNaviT {runNaviT :: m a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadNotify,
      MonadShell
    )
    via m

instance MonadTrans NaviT where
  lift = MkNaviT

instance MonadMutRef m ref => MonadMutRef (NaviT m) ref where
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref = lift . writeRef ref

newtype FatalErr = MkFatalErr {unFatalErr :: Text}
  deriving (Show)

runNavi ::
  forall ref m.
  ( MonadMutRef m ref,
    MonadNotify m,
    MonadShell m
  ) =>
  Args Identity ->
  m (Either FatalErr Void)
runNavi args = prepareNavi @ref args >>= traverse processEvents

prepareNavi ::
  forall ref m.
  ( MonadMutRef m ref,
    MonadNotify m,
    MonadShell m
  ) =>
  Args Identity ->
  m (Either FatalErr (Config m ref, Client))
prepareNavi args =
  first MkFatalErr <$> (liftA2 (,) <$> tryParseConfig args <*> tryInit)

processEvents :: (MonadMutRef m ref, MonadNotify m, MonadShell m) => (Config m ref, Client) -> m Void
processEvents (config, client) = forever $ do
  sleep $ pollInterval config
  traverse (processEvent client) (events config)

processEvent :: (MonadMutRef m ref, MonadNotify m) => Client -> Event m ref -> m ()
processEvent client MkEvent {trigger, errorEvent} = trigger >>= handleResult
  where
    handleResult None = pure ()
    handleResult (Err err) = do
      blockErrEvent <- Event.blockErr errorEvent
      --putStrLn $ "Event Error: " <> showt err
      if blockErrEvent
        then pure ()
        else sendNote client (serviceErrToNote err)
    handleResult (Alert nt) = sendNote client nt

serviceErrToNote :: EventErr -> Note
serviceErrToNote (MkEventErr nm short _) = Event.mkNote Nothing summary body hints timeout
  where
    summary = "Event Error"
    body = Just $ Text $ T.unpack $ nm <> ": " <> short
    hints = [Urgency Critical]
    timeout = Milliseconds 10_000

tryInit :: MonadNotify m => m (Either Text Client)
tryInit = do
  eitherClient <- initConn
  pure $ case eitherClient of
    Left err -> Left $ mkErr err
    Right c -> Right c
  where
    mkErr = (<>) "Error initiating notifications: " . T.pack . displayException

tryParseConfig :: (MonadMutRef m ref, MonadShell m) => Args Identity -> m (Either Text (Config m ref))
tryParseConfig =
  fmap (first mkErr)
    . readConfig
    . runIdentity
    . configFile
  where
    mkErr = (<>) "Config error: " . showt
