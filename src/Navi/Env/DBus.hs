{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides environment for usage with DBus.
module Navi.Env.DBus
  ( -- * Environment
    HasDBusClient (..),
    DBusEnv (..),

    -- * Creation

    -- ** Effect
    DBusCreateStatic,

    -- *** Handler
    runMkDbusEnvIO,

    -- * Functions
    mkDBusEnv,
    --- * Misc
    naviToDBus,
  )
where

import DBus.Client (Client)
import DBus.Notify (Hint (Urgency), Note)
import DBus.Notify qualified as DBusN
import Effectful.Dispatch.Static
  ( SideEffects (WithSideEffects),
    StaticRep,
    evalStaticRep,
    unsafeEff_,
  )
import Navi.Config (Config)
import Navi.Data.NaviLog (LogEnv)
import Navi.Data.NaviNote (NaviNote, Timeout (Never, Seconds))
import Navi.Env.Core
  ( Env (MkEnv),
    HasEvents (getEvents),
    HasLogEnv (getLogEnv, localLogEnv),
    HasNoteQueue (getNoteQueue),
  )
import Navi.Prelude

-- | Retrieves the notification client.
class HasDBusClient env where
  getClient :: env -> Client

-- | Concrete dbus environment. Adds the dbus client.
data DBusEnv = MkDBusEnv
  { coreEnv :: !Env,
    dbusClient :: !Client
  }

makeFieldLabelsNoPrefix ''DBusEnv

instance HasEvents DBusEnv where
  getEvents = view (#coreEnv % #events)

instance HasLogEnv DBusEnv where
  getLogEnv = view (#coreEnv % #logEnv)
  localLogEnv = over' (#coreEnv % #logEnv)

instance HasNoteQueue DBusEnv where
  getNoteQueue = view (#coreEnv % #noteQueue)

instance HasDBusClient DBusEnv where
  getClient = view #dbusClient

-- | Static effect for creating a dbus env.
--
-- @since 0.1
data DBusCreateStatic :: Effect

type instance DispatchOf DBusCreateStatic = Static WithSideEffects

data instance StaticRep DBusCreateStatic = MkDBusCreateStatic

-- | Handler for creating the environment.
runMkDbusEnvIO ::
  (IOE :> es) =>
  Eff (DBusCreateStatic : es) a ->
  Eff es a
runMkDbusEnvIO = evalStaticRep MkDBusCreateStatic

-- | Creates a 'DBusEnv' from the provided log types and configuration data.
mkDBusEnv ::
  ( Concurrent :> es,
    DBusCreateStatic :> es
  ) =>
  LogEnv ->
  Config ->
  Eff es DBusEnv
mkDBusEnv logEnv config = do
  client <- unsafeEff_ DBusN.connectSession
  noteQueue <- newTBQueueA 1000
  pure
    $ MkDBusEnv
      { coreEnv =
          MkEnv
            (config ^. #events)
            logEnv
            noteQueue,
        dbusClient = client
      }
{-# INLINEABLE mkDBusEnv #-}

-- | Turns a 'NaviNote' into a DBus 'Note'.
naviToDBus :: NaviNote -> Note
naviToDBus naviNote =
  DBusN.Note
    { appName = "Navi",
      summary = unpack $ naviNote ^. #summary,
      body = body,
      appImage = Nothing,
      hints = hints,
      expiry = timeout,
      actions = []
    }
  where
    body = DBusN.Text . unpack <$> naviNote ^. #body
    hints = maybeToList $ Urgency <$> naviNote ^. #urgency
    timeout = maybe defTimeout naviToDBusTimeout $ naviNote ^. #timeout
    defTimeout = DBusN.Milliseconds 10_000

naviToDBusTimeout :: Timeout -> DBusN.Timeout
naviToDBusTimeout Never = DBusN.Never
naviToDBusTimeout (Seconds s) = DBusN.Milliseconds $ (* 1_000) $ w16ToInt32 s
  where
    w16ToInt32 :: Word16 -> Int32
    w16ToInt32 = fromIntegral
