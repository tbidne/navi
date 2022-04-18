module Unit.Prelude
  ( module X,
    decodeExpected,
  )
where

import Navi.Prelude as X
import Test.Tasty as X (TestTree, testGroup)
import Test.Tasty.HUnit as X (assertBool, assertFailure, testCase, (@=?))
import Toml (TomlCodec)
import Toml qualified

-- | Attempts to parse the parameter text by the given codec then verify that
-- it matches the expected value. If either the parsing or verification fails,
-- then we give an assertion failure.
--
-- @since 0.1
decodeExpected ::
  (Eq a, Show a) =>
  TomlCodec toml ->
  String ->
  Text ->
  a ->
  (toml -> a) ->
  TestTree
decodeExpected codec desc txt expected fromToml = testCase desc $ do
  let eResult = Toml.decodeExact codec txt
  case eResult of
    Left err -> assertFailure $ "Parsing config fails: " <> show err
    Right result -> expected @=? fromToml result
