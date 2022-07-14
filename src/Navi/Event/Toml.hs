{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This modules provides toml configuration related to events.
module Navi.Event.Toml
  ( RepeatEventToml (..),
    _NoRepeatsToml,
    _AllowRepeatsToml,
    repeatEventOptDecoder,
    repeatEventTomlToVal,
    mRepeatEventTomlToVal,
    ErrorNoteToml (..),
    _NoErrNoteToml,
    _ErrNoteAllowRepeatsToml,
    _ErrNoteNoRepeatsToml,
    errorNoteOptDecoder,
    errorNoteTomlToVal,
    mErrorNoteTomlToVal,
  )
where

import Navi.Effects.MonadMutRef (MonadMutRef (..))
import Navi.Event.Types (ErrorNote (..), RepeatEvent (..))
import Navi.Prelude

-- | TOML for 'RepeatEvent'.
data RepeatEventToml
  = NoRepeatsToml
  | AllowRepeatsToml
  deriving stock (Eq, Show)

makePrisms ''RepeatEventToml

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
repeatEventTomlToVal :: MonadMutRef ref m => RepeatEventToml -> m (RepeatEvent ref a)
repeatEventTomlToVal AllowRepeatsToml = pure AllowRepeats
repeatEventTomlToVal NoRepeatsToml = NoRepeats <$> newRef Nothing
{-# INLINEABLE repeatEventTomlToVal #-}

-- | Constructs a mutable 'RepeatEvent' from 'RepeatEventToml'. If none is
-- provided, defaults to 'NoRepeatsToml', i.e., no repeats.
mRepeatEventTomlToVal :: MonadMutRef ref m => Maybe RepeatEventToml -> m (RepeatEvent ref a)
mRepeatEventTomlToVal Nothing = repeatEventTomlToVal NoRepeatsToml
mRepeatEventTomlToVal (Just t) = repeatEventTomlToVal t
{-# INLINEABLE mRepeatEventTomlToVal #-}

-- | TOML for 'ErrorNote'.
data ErrorNoteToml
  = NoErrNoteToml
  | ErrNoteAllowRepeatsToml
  | ErrNoteNoRepeatsToml
  deriving stock (Eq, Show)

makePrisms ''ErrorNoteToml

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
errorNoteTomlToVal :: MonadMutRef ref m => ErrorNoteToml -> m (ErrorNote ref)
errorNoteTomlToVal NoErrNoteToml = pure NoErrNote
errorNoteTomlToVal ErrNoteAllowRepeatsToml = pure $ AllowErrNote AllowRepeats
errorNoteTomlToVal ErrNoteNoRepeatsToml = AllowErrNote . NoRepeats <$> newRef Nothing
{-# INLINEABLE errorNoteTomlToVal #-}

-- | Constructs a mutable 'ErrorNote' from 'ErrorNoteToml'. If none is
-- provided, defaults to 'ErrNoteNoRepeatsToml', i.e., we /do/ send
-- notifications for errors, but we do not send repeats.
mErrorNoteTomlToVal :: MonadMutRef ref m => Maybe ErrorNoteToml -> m (ErrorNote ref)
mErrorNoteTomlToVal Nothing = errorNoteTomlToVal ErrNoteNoRepeatsToml
mErrorNoteTomlToVal (Just t) = errorNoteTomlToVal t
{-# INLINEABLE mErrorNoteTomlToVal #-}
