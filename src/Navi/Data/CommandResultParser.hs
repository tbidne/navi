{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Navi.Data.CommandResultParser
  ( CommandResultParserToml (..),
    CommandResultParser (..),
    commandResultParserDecoder,
    defaultParser,
  )
where

import Data.Text qualified as T
import Navi.Data.CommandResult
import Navi.Event.Types.EventError (EventError (MkEventError, long, name, short))
import Navi.Prelude

-- | Parses a text command result
newtype CommandResultParser = MkCommandResultParser
  { unCommandResultParser :: Text -> Either EventError CommandResult
  }

makeFieldLabelsNoPrefix ''CommandResultParser

instance Show CommandResultParser where
  show _ = "<parser>"

instance Eq CommandResultParser where
  _ == _ = True

-- | Toml
data CommandResultElem
  = CommandResultElemTrigger
  | CommandResultElemOutput
  deriving stock (Eq, Show)

parseResultType :: (MonadFail m) => Text -> m CommandResultElem
parseResultType "trigger" = pure CommandResultElemTrigger
parseResultType "output" = pure CommandResultElemOutput
parseResultType other =
  fail
    $ mconcat
      [ "Unrecognized command-result: '",
        unpackText other,
        "'"
      ]

-- | Exists so that we can attach the DecodeTOML instance. The text argument
-- is the service's name, which we later use when throwing parse errors.
newtype CommandResultParserToml = MkCommandResultParserToml (Text -> CommandResultParser)

instance Show CommandResultParserToml where
  show _ = "<parser>"

instance Eq CommandResultParserToml where
  _ == _ = True

instance DecodeTOML CommandResultParserToml where
  tomlDecoder = do
    txt <- tomlDecoder

    let isParens = checkParens txt
        numCommas = countCommas txt

    -- Manually pick parsers rather than just combining with asum so we can
    -- get better error messages.
    r <-
      if
        | isParens && numCommas == 0 -> parseCommand1Tuple txt
        | isParens && numCommas == 1 -> parseCommand2 txt
        | otherwise -> parseCommand1Literal txt

    pure $ MkCommandResultParserToml r
    where
      checkParens txt =
        "("
          `T.isPrefixOf` txt
          && ")"
          `T.isSuffixOf` txt

      countCommas = T.count ","

commandResultParserDecoder :: Decoder (Maybe CommandResultParserToml)
commandResultParserDecoder = getFieldOptWith tomlDecoder "command-result"

defaultParser :: CommandResultParser
defaultParser = MkCommandResultParser $ \result ->
  Right
    $ MkCommandResult
      { result,
        output = Nothing,
        pollInterval = Nothing
      }

parseCommand1Literal :: (MonadFail m) => Text -> m (Text -> CommandResultParser)
parseCommand1Literal txt = do
  parseResultType txt >>= \case
    CommandResultElemTrigger -> pure $ const defaultParser
    CommandResultElemOutput -> fail "A single command result can only be 'trigger'."

parseCommand1Tuple :: (MonadFail m) => Text -> m (Text -> CommandResultParser)
parseCommand1Tuple txt = do
  inner <- parseTuple txt
  parseCommand1Literal inner

-- On the Multiple/Single DecodeTOML instance, but passed to MonadSystemInfo
-- as part of the Service Type.
--
-- So the service type has this parser on it. We are going to want the
-- name.
parseCommand2 :: (MonadFail m) => Text -> m (Text -> CommandResultParser)
parseCommand2 txt = do
  (x1, x2) <- parseTuple2 txt

  o1 <- parseResultType x1
  o2 <- parseResultType x2

  case (o1, o2) of
    (CommandResultElemTrigger, CommandResultElemTrigger) -> fail "Found trigger twice, expected exactly one."
    (CommandResultElemTrigger, CommandResultElemOutput) ->
      pure $ \name -> MkCommandResultParser $ \r -> do
        case parseTuple2 r of
          Just (result, output) ->
            Right
              $ MkCommandResult
                { result,
                  output = Just output,
                  pollInterval = Nothing
                }
          Nothing ->
            Left
              $ MkEventError
                { name,
                  short = "Parse error",
                  long = "Failed to parse (trigger, output): " <> r
                }
    (CommandResultElemOutput, CommandResultElemTrigger) ->
      pure $ \name -> MkCommandResultParser $ \r -> do
        case parseTuple2 r of
          Just (output, result) ->
            Right
              $ MkCommandResult
                { result,
                  output = Just output,
                  pollInterval = Nothing
                }
          Nothing ->
            Left
              $ MkEventError
                { name,
                  short = "Parse error",
                  long = "Failed to parse (output, trigger): " <> r
                }
    (CommandResultElemOutput, CommandResultElemOutput) -> fail "Found output twice, expected at most one."

parseTuple :: (MonadFail m) => Text -> m Text
parseTuple txt = do
  r1 <- liftMonadFail "Expected '('" $ T.stripPrefix "(" txt
  let (inner, rest) = T.break (== ')') r1
  unless (rest == ")") (fail "Expected closing ')'")
  pure inner

parseTuple2 :: (MonadFail m) => Text -> m (Text, Text)
parseTuple2 txt = do
  r1 <- parseTuple txt
  case T.split (== ',') r1 of
    [] -> fail "Expected 2-tuple, found 0 elements"
    [_] -> fail "Expected 2-tuple, found 1 element"
    [l, r] -> pure (T.strip l, T.strip r)
    _ -> fail $ "Expected 2-tuple, found > 2 elements: " ++ unpackText txt

liftMonadFail :: (MonadFail m) => String -> Maybe a -> m a
liftMonadFail _ (Just x) = pure x
liftMonadFail msg Nothing = fail msg
