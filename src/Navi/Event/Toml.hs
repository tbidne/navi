{-# LANGUAGE UndecidableInstances #-}

-- | This modules provides toml configuration related to events.
module Navi.Event.Toml
  ( RepeatEventToml (..),
    repeatEventOptDecoder,
    repeatEventTomlToVal,
    mRepeatEventTomlToVal,
    ErrorNoteToml (..),
    errorNoteOptDecoder,
    errorNoteTomlToVal,
    mErrorNoteTomlToVal,
  )
where

import Navi.Event.Types (ErrorNote (..), RepeatEvent (..))
import Navi.Prelude

-- | TOML for 'RepeatEvent'.
data RepeatEventToml
  = NoRepeatsToml
  | AllowRepeatsToml
  deriving stock (Eq, Show)

-- | @since 0.1
instance DecodeTOML RepeatEventToml where
  tomlDecoder =
    tomlDecoder <&> \b ->
      if b
        then AllowRepeatsToml
        else NoRepeatsToml

-- | TOML decoder for optional 'RepeatEventToml' with field name
-- "repeat-events".
--
-- @since 0.1
repeatEventOptDecoder :: Decoder (Maybe RepeatEventToml)
repeatEventOptDecoder = getFieldOptWith tomlDecoder "repeat-events"

-- | Constructs a mutable 'RepeatEvent' from 'RepeatEventToml'.
repeatEventTomlToVal :: (MonadIORef m) => RepeatEventToml -> m (RepeatEvent a)
repeatEventTomlToVal AllowRepeatsToml = pure AllowRepeats
repeatEventTomlToVal NoRepeatsToml = NoRepeats <$> newIORef Nothing
{-# INLINEABLE repeatEventTomlToVal #-}

-- | Constructs a mutable 'RepeatEvent' from 'RepeatEventToml'. If none is
-- provided, defaults to 'NoRepeatsToml', i.e., no repeats.
mRepeatEventTomlToVal :: (MonadIORef m) => Maybe RepeatEventToml -> m (RepeatEvent a)
mRepeatEventTomlToVal Nothing = repeatEventTomlToVal NoRepeatsToml
mRepeatEventTomlToVal (Just t) = repeatEventTomlToVal t
{-# INLINEABLE mRepeatEventTomlToVal #-}

-- | TOML for 'ErrorNote'.
data ErrorNoteToml
  = NoErrNoteToml
  | ErrNoteAllowRepeatsToml
  | ErrNoteNoRepeatsToml
  deriving stock (Eq, Show)

-- | @since 0.1
instance DecodeTOML ErrorNoteToml where
  tomlDecoder =
    tomlDecoder
      >>= \case
        "none" -> pure NoErrNoteToml
        "repeats" -> pure ErrNoteAllowRepeatsToml
        "no-repeats" -> pure ErrNoteNoRepeatsToml
        bad ->
          fail $
            unpack $
              concat
                [ "Unexpected error-events string: ",
                  bad,
                  ". Expected one of <none | repeats | no-repeats>."
                ]

-- | TOML decoder for optional 'ErrorNoteToml' with field name
-- "error-events".
--
-- @since 0.1
errorNoteOptDecoder :: Decoder (Maybe ErrorNoteToml)
errorNoteOptDecoder = getFieldOptWith tomlDecoder "error-events"

-- | Constructs a mutable 'ErrorNote' from 'ErrorNoteToml'.
errorNoteTomlToVal :: (MonadIORef m) => ErrorNoteToml -> m ErrorNote
errorNoteTomlToVal NoErrNoteToml = pure NoErrNote
errorNoteTomlToVal ErrNoteAllowRepeatsToml = pure $ AllowErrNote AllowRepeats
errorNoteTomlToVal ErrNoteNoRepeatsToml = AllowErrNote . NoRepeats <$> newIORef Nothing
{-# INLINEABLE errorNoteTomlToVal #-}

-- | Constructs a mutable 'ErrorNote' from 'ErrorNoteToml'. If none is
-- provided, defaults to 'ErrNoteNoRepeatsToml', i.e., we /do/ send
-- notifications for errors, but we do not send repeats.
mErrorNoteTomlToVal :: (MonadIORef m) => Maybe ErrorNoteToml -> m ErrorNote
mErrorNoteTomlToVal Nothing = errorNoteTomlToVal ErrNoteNoRepeatsToml
mErrorNoteTomlToVal (Just t) = errorNoteTomlToVal t
{-# INLINEABLE mErrorNoteTomlToVal #-}
