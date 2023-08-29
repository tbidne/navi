{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Provides an effect for sending system notifications.
module Navi.Effectful.Notify
  ( -- * Effect
    NotifyDynamic (..),
    sendNote,

    -- ** Handlers
    runNotifyDynamicDBusIO,
    runNotifyDynamicNotifySendIO,

    -- * Functions
    sendNoteQueue,
  )
where

import DBus.Notify qualified as DBusN
import Effectful.Dispatch.Dynamic (send)
import Effectful.LoggerNS.Dynamic (addNamespace)
import Effectful.Process.Typed qualified as TP
import Navi.Data.NaviNote (NaviNote)
import Navi.Env.Core (HasNoteQueue (getNoteQueue))
import Navi.Env.DBus (DBusEnv, HasDBusClient (getClient), naviToDBus)
import Navi.Env.NotifySend (naviToNotifySend)
import Navi.Prelude

-- | This effect represents sending desktop notifications.
data NotifyDynamic :: Effect where
  SendNote :: NaviNote -> NotifyDynamic m ()

-- | @since 0.1
type instance DispatchOf NotifyDynamic = Dynamic

sendNote :: (NotifyDynamic :> es) => NaviNote -> Eff es ()
sendNote = send . SendNote

runNotifyDynamicDBusIO ::
  ( IOE :> es,
    LoggerDynamic :> es,
    LoggerNSDynamic :> es,
    Reader DBusEnv :> es
  ) =>
  Eff (NotifyDynamic : es) a ->
  Eff es a
runNotifyDynamicDBusIO = interpret $ \_ -> \case
  SendNote naviNote -> addNamespace "dbus" $ do
    $(logDebug) (showt note)
    client <- asks @DBusEnv getClient
    liftIO $ sendDbus client note
    where
      note = naviToDBus naviNote
      sendDbus c = void . DBusN.notify c

runNotifyDynamicNotifySendIO ::
  ( LoggerDynamic :> es,
    LoggerNSDynamic :> es,
    TypedProcess :> es
  ) =>
  Eff (NotifyDynamic : es) a ->
  Eff es a
runNotifyDynamicNotifySendIO = interpret $ \_ -> \case
  SendNote naviNote -> addNamespace "notify-send" $ do
    $(logDebug) noteTxt
    void $ TP.runProcess cp
    where
      noteTxt = naviToNotifySend naviNote
      cp = TP.shell $ unpack noteTxt

-- | Convenience function for retrieving a 'TBQueue'
-- 'NaviNote' from the @env@ and sending the note.
sendNoteQueue ::
  forall env es.
  ( Concurrent :> es,
    HasNoteQueue env,
    Reader env :> es
  ) =>
  NaviNote ->
  Eff es ()
sendNoteQueue naviNote =
  asks @env (getNoteQueue @env) >>= (`writeTBQueueA` naviNote)
{-# INLINEABLE sendNoteQueue #-}
