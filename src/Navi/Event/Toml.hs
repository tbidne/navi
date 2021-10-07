module Navi.Event.Toml
  ( commandCodec,
    RepeatEvtToml (..),
    repeatEvtCodec,
    repeatEvtTomlToVal,
    mRepeatEvtTomlToVal,
    ErrorNoteToml (..),
    errorNoteCodec,
    errorNoteTomlToVal,
    mErrorNoteTomlToVal,
  )
where

import Data.IORef qualified as IORef
import Data.Text qualified as T
import Navi.Event.Types (Command (..), ErrorNote (..), RepeatEvent (..))
import Navi.Prelude
import Toml (TomlCodec)
import Toml qualified

data EventConfig = MkEventConfig
  { repeatEvt :: Maybe RepeatEvtToml,
    errEvt :: Maybe ErrorNoteToml
  }
  deriving (Show)

commandCodec :: TomlCodec Command
commandCodec = Toml.textBy (T.pack . show) (Right . MkCommand) "command"

data RepeatEvtToml
  = NoRepeatsToml
  | AllowRepeatsToml
  deriving (Show)

repeatEvtCodec :: TomlCodec RepeatEvtToml
repeatEvtCodec = Toml.dimap toBool fromBool $ Toml.bool "repeat-events"
  where
    fromBool True = AllowRepeatsToml
    fromBool False = NoRepeatsToml
    toBool AllowRepeatsToml = True
    toBool NoRepeatsToml = False

repeatEvtTomlToVal :: RepeatEvtToml -> IO (RepeatEvent a)
repeatEvtTomlToVal AllowRepeatsToml = pure AllowRepeats
repeatEvtTomlToVal NoRepeatsToml = NoRepeats <$> IORef.newIORef Nothing

mRepeatEvtTomlToVal :: Maybe RepeatEvtToml -> IO (RepeatEvent a)
mRepeatEvtTomlToVal Nothing = repeatEvtTomlToVal NoRepeatsToml
mRepeatEvtTomlToVal (Just t) = repeatEvtTomlToVal t

data ErrorNoteToml
  = NoErrNoteToml
  | ErrNoteAllowRepeatsToml
  | ErrNoteNoRepeatsToml
  deriving (Show)

errorNoteCodec :: TomlCodec ErrorNoteToml
errorNoteCodec = Toml.textBy showErrEvt parseErrEvt "error-events"
  where
    showErrEvt NoErrNoteToml = "none"
    showErrEvt ErrNoteAllowRepeatsToml = "repeats"
    showErrEvt ErrNoteNoRepeatsToml = "no-repeats"
    parseErrEvt "none" = Right NoErrNoteToml
    parseErrEvt "repeats" = Right NoErrNoteToml
    parseErrEvt "no-repeats" = Right NoErrNoteToml
    parseErrEvt other = Left $ "Unsupported error event key: " <> other

errorNoteTomlToVal :: ErrorNoteToml -> IO ErrorNote
errorNoteTomlToVal NoErrNoteToml = pure NoErrNote
errorNoteTomlToVal ErrNoteAllowRepeatsToml = pure $ AllowErrNote AllowRepeats
errorNoteTomlToVal ErrNoteNoRepeatsToml = AllowErrNote . NoRepeats <$> IORef.newIORef Nothing

mErrorNoteTomlToVal :: Maybe ErrorNoteToml -> IO ErrorNote
mErrorNoteTomlToVal Nothing = errorNoteTomlToVal ErrNoteNoRepeatsToml
mErrorNoteTomlToVal (Just t) = errorNoteTomlToVal t
