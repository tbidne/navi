module Navi
  ( NaviT (..),
    runNavi,
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT, asks)
import Control.Monad.Trans (MonadTrans (..))
import DBus.Notify (Body (..), Client, Hint (..), Note, Timeout (..), UrgencyLevel (..))
import Data.Text qualified as T
import Data.Void (Void)
import Navi.Effects (MonadMutRef (..), MonadNotify (..), MonadShell (..))
import Navi.Env (Env (..))
import Navi.Event
  ( AnyEvent (..),
    Event (..),
    EventErr (..),
  )
import Navi.Event qualified as Event
import Navi.Prelude

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

instance MonadMutRef m ref => MonadMutRef (NaviT e m) ref where
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref = lift . writeRef ref

newtype FatalErr = MkFatalErr {unFatalErr :: Text}
  deriving (Show)

runNavi ::
  forall ref m.
  ( MonadMutRef m ref,
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
    traverse (processEvent client) events

processEvent :: (MonadMutRef m ref, MonadNotify m, MonadShell m) => Client -> AnyEvent ref -> m ()
processEvent client (MkAnyEvent event@MkEvent {repeatEvent, errorNote, raiseAlert}) = Event.runEvent event >>= handleResult
  where
    handleResult (Left err) = do
      blockErrEvent <- Event.blockErr errorNote
      --putStrLn $ "Event Error: " <> showt err
      if blockErrEvent
        then pure ()
        else sendNote client (serviceErrToNote err)
    handleResult (Right result) = do
      case raiseAlert result of
        Nothing -> Event.updatePrevTrigger repeatEvent result $> ()
        Just note -> do
          blocked <- Event.blockRepeat repeatEvent result
          if blocked
            then pure ()
            else do
              Event.updatePrevTrigger repeatEvent result
              sendNote client note

serviceErrToNote :: EventErr -> Note
serviceErrToNote (MkEventErr nm short _) = Event.mkNote Nothing summary body hints timeout
  where
    summary = "Event Error"
    body = Just $ Text $ T.unpack $ nm <> ": " <> short
    hints = [Urgency Critical]
    timeout = Milliseconds 10_000
