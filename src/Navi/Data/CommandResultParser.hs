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
  ( CommandResult
      ( MkCommandResult,
        output,
        pollInterval,
        result
      ),
  )
import Navi.Data.PollInterval (parsePollInterval)
import Navi.Data.Result (Result (Err, Ok), ResultDefault)
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
  = CommandResultElemPollInterval
  | CommandResultElemTrigger
  | CommandResultElemOutput
  deriving stock (Eq, Show)

displayElem :: CommandResultElem -> String
displayElem CommandResultElemPollInterval = "poll-interval"
displayElem CommandResultElemTrigger = "trigger"
displayElem CommandResultElemOutput = "output"

displayElems2 :: (CommandResultElem, CommandResultElem) -> String
displayElems2 (x1, x2) = "(" ++ displayElem x1 ++ ", " ++ displayElem x2 ++ ")"

displayElems3 :: (CommandResultElem, CommandResultElem, CommandResultElem) -> String
displayElems3 (x1, x2, x3) =
  "("
    ++ displayElem x1
    ++ ", "
    ++ displayElem x2
    ++ ", "
    ++ displayElem x3
    ++ ")"

parseResultType :: (MonadFail m) => Text -> m CommandResultElem
parseResultType "poll-interval" = pure CommandResultElemPollInterval
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
newtype CommandResultParserToml = MkCommandResultParserToml
  { unCommandResultParserToml :: Text -> CommandResultParser
  }

makeFieldLabelsNoPrefix ''CommandResultParserToml

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
        | isParens && numCommas == 2 -> parseCommand3 txt
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
    _ ->
      fail
        $ "A single command result can only be 'trigger', found: "
        <> unpackText txt

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
    (CommandResultElemTrigger, CommandResultElemOutput) ->
      pure $ \name -> MkCommandResultParser $ \r -> do
        (result, output) <-
          liftParse name "Failed to parse (trigger, output)" $ parseTuple2 r
        pure
          $ MkCommandResult
            { result,
              output = Just output,
              pollInterval = Nothing
            }
    (CommandResultElemTrigger, CommandResultElemPollInterval) ->
      pure $ \name -> MkCommandResultParser $ \r -> do
        let desc = "Failed to parse (trigger, poll-interval)"
        (result, pollIntervalTxt) <- liftParse name desc $ parseTuple2 r
        pollInterval <- liftParse name desc $ parsePollInterval pollIntervalTxt
        pure
          $ MkCommandResult
            { result,
              output = Nothing,
              pollInterval = Just pollInterval
            }
    (CommandResultElemOutput, CommandResultElemTrigger) ->
      pure $ \name -> MkCommandResultParser $ \r -> do
        (output, result) <-
          liftParse name "Failed to parse (output, trigger)" $ parseTuple2 r
        pure
          $ MkCommandResult
            { result,
              output = Just output,
              pollInterval = Nothing
            }
    (CommandResultElemPollInterval, CommandResultElemTrigger) ->
      pure $ \name -> MkCommandResultParser $ \r -> do
        let desc = "Failed to parse (poll-interval, trigger)"
        (pollIntervalTxt, result) <- liftParse name desc $ parseTuple2 r
        pollInterval <- liftParse name desc $ parsePollInterval pollIntervalTxt
        pure
          $ MkCommandResult
            { result,
              output = Nothing,
              pollInterval = Just pollInterval
            }
    _ ->
      fail $ "Expected one 'trigger' and one more element, found: " ++ displayElems2 (o1, o2)

parseCommand3 :: (MonadFail m) => Text -> m (Text -> CommandResultParser)
parseCommand3 txt = do
  (x1, x2, x3) <- parseTuple3 txt

  o1 <- parseResultType x1
  o2 <- parseResultType x2
  o3 <- parseResultType x3

  case (o1, o2, o3) of
    -- T O P
    (CommandResultElemTrigger, CommandResultElemOutput, CommandResultElemPollInterval) ->
      pure $ \name -> MkCommandResultParser $ \r -> do
        let desc = "Failed to parse (trigger, output, poll-interval)"
        (result, output, pollIntervalTxt) <- liftParse name desc $ parseTuple3 r
        pollInterval <- liftParse name desc $ parsePollInterval pollIntervalTxt
        pure
          $ MkCommandResult
            { result,
              output = Just output,
              pollInterval = Just pollInterval
            }
    -- T P O
    (CommandResultElemTrigger, CommandResultElemPollInterval, CommandResultElemOutput) ->
      pure $ \name -> MkCommandResultParser $ \r -> do
        let desc = "Failed to parse (trigger, poll-interval, output)"
        (result, pollIntervalTxt, output) <- liftParse name desc $ parseTuple3 r
        pollInterval <- liftParse name desc $ parsePollInterval pollIntervalTxt
        pure
          $ MkCommandResult
            { result,
              output = Just output,
              pollInterval = Just pollInterval
            }
    -- O T P
    (CommandResultElemOutput, CommandResultElemTrigger, CommandResultElemPollInterval) ->
      pure $ \name -> MkCommandResultParser $ \r -> do
        let desc = "Failed to parse (output, trigger, poll-interval)"
        (output, result, pollIntervalTxt) <- liftParse name desc $ parseTuple3 r
        pollInterval <- liftParse name desc $ parsePollInterval pollIntervalTxt
        pure
          $ MkCommandResult
            { result,
              output = Just output,
              pollInterval = Just pollInterval
            }
    -- O P T
    (CommandResultElemOutput, CommandResultElemPollInterval, CommandResultElemTrigger) ->
      pure $ \name -> MkCommandResultParser $ \r -> do
        let desc = "Failed to parse (output, poll-interval, trigger)"
        (output, pollIntervalTxt, result) <- liftParse name desc $ parseTuple3 r
        pollInterval <- liftParse name desc $ parsePollInterval pollIntervalTxt
        pure
          $ MkCommandResult
            { result,
              output = Just output,
              pollInterval = Just pollInterval
            }
    -- P T O
    (CommandResultElemPollInterval, CommandResultElemTrigger, CommandResultElemOutput) ->
      pure $ \name -> MkCommandResultParser $ \r -> do
        let desc = "Failed to parse (poll-interval, trigger, output)"
        (pollIntervalTxt, result, output) <- liftParse name desc $ parseTuple3 r
        pollInterval <- liftParse name desc $ parsePollInterval pollIntervalTxt
        pure
          $ MkCommandResult
            { result,
              output = Just output,
              pollInterval = Just pollInterval
            }
    -- P O T
    (CommandResultElemPollInterval, CommandResultElemOutput, CommandResultElemTrigger) ->
      pure $ \name -> MkCommandResultParser $ \r -> do
        let desc = "Failed to parse (poll-interval, output, trigger)"
        (pollIntervalTxt, output, result) <- liftParse name desc $ parseTuple3 r
        pollInterval <- liftParse name desc $ parsePollInterval pollIntervalTxt
        pure
          $ MkCommandResult
            { result,
              output = Just output,
              pollInterval = Just pollInterval
            }
    _ ->
      fail $ "Expected 3 different elements, found: " ++ displayElems3 (o1, o2, o3)

parseTuple :: (MonadFail m) => Text -> m Text
parseTuple txt = do
  r1 <- liftMonadFail ("Expected '(', received: " <> unpackText txt) $ T.stripPrefix "(" txt
  let (inner, rest) = T.break (== ')') r1
      rest' = T.strip rest
      msg =
        mconcat
          [ "Expected closing ')', received: '",
            rest',
            "'"
          ]
  unless (rest' == ")") (fail $ unpackText msg)
  pure inner

parseTuple2 :: (MonadFail m) => Text -> m (Text, Text)
parseTuple2 txt = do
  r1 <- parseTuple txt
  case T.split (== ',') r1 of
    [] -> fail "Expected 2-tuple, found 0 elements"
    [_] -> fail $ "Expected 2-tuple, found 1 element: " ++ unpackText txt
    [l, r] -> pure (T.strip l, T.strip r)
    _ -> fail $ "Expected 2-tuple, found > 2 elements: " ++ unpackText txt

parseTuple3 :: (MonadFail m) => Text -> m (Text, Text, Text)
parseTuple3 txt = do
  r1 <- parseTuple txt
  case T.split (== ',') r1 of
    [] -> fail "Expected 3-tuple, found 0 elements"
    [_] -> fail $ "Expected 3-tuple, found 1 element: " ++ unpackText txt
    [_, _] -> fail $ "Expected 3-tuple, found 2 elements: " ++ unpackText txt
    [x1, x2, x3] -> pure (T.strip x1, T.strip x2, T.strip x3)
    _ -> fail $ "Expected 3-tuple, found > 3 elements: " ++ unpackText txt

liftMonadFail :: (MonadFail m) => String -> Maybe a -> m a
liftMonadFail _ (Just x) = pure x
liftMonadFail msg Nothing = fail msg

liftParse :: Text -> Text -> ResultDefault a -> Either EventError a
liftParse name msg (Err err) =
  Left
    $ MkEventError
      { name,
        short = msg,
        long = packText err
      }
liftParse _ _ (Ok x) = Right x
