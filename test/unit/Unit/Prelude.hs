{-# LANGUAGE AllowAmbiguousTypes #-}

module Unit.Prelude
  ( module X,

    -- * HUnit
    decodeExpected,
    decodeExpectedFailure,
    assertDecode,
    assertDecodeFailure,
    assertLeft,
    assertRight,

    -- * Hedgehog
    testProp,
    hassertLeft,
    hassertRight,
  )
where

import Hedgehog as X
  ( Gen,
    PropertyName,
    PropertyT,
    annotate,
    annotateShow,
    failure,
    forAll,
    (===),
  )
import Hedgehog qualified as H
import Hedgehog.Range as X (Range)
import Navi.Prelude as X
import Test.Tasty as X (TestName, TestTree, testGroup)
import Test.Tasty.HUnit as X (Assertion, assertBool, assertFailure, testCase, (@=?))
import Test.Tasty.Hedgehog qualified as TH

-- | Attempts to parse the parameter text by the given codec then verify that
-- it matches the expected value. If either the parsing or verification fails,
-- then we give an assertion failure.
--
-- @since 0.1
decodeExpected ::
  (DecodeTOML toml, Eq a, Show a) =>
  String ->
  Text ->
  a ->
  (toml -> a) ->
  TestTree
decodeExpected desc txt expected onToml =
  assertDecode desc txt (\toml -> expected @=? onToml toml)

decodeExpectedFailure ::
  forall toml.
  (DecodeTOML toml, Show toml) =>
  String ->
  Text ->
  String ->
  TestTree
decodeExpectedFailure desc txt expected =
  assertDecodeFailure @toml desc txt (\err -> expected @=? displayException err)

assertDecode ::
  (DecodeTOML toml) =>
  String ->
  Text ->
  (toml -> Assertion) ->
  TestTree
assertDecode desc txt onToml = testCase desc $ do
  case decode txt of
    Left err -> assertFailure $ "Parsing config fails: " <> show err
    Right result -> onToml result

assertDecodeFailure ::
  forall toml.
  (DecodeTOML toml, Show toml) =>
  String ->
  Text ->
  (TOMLError -> Assertion) ->
  TestTree
assertDecodeFailure desc txt onErr = testCase desc $ do
  case decode @toml txt of
    Left err -> onErr err
    Right result -> assertFailure $ "Parsing config succeeded: " <> show result

assertLeft :: (Show a) => Either e a -> IO e
assertLeft (Left x) = pure x
assertLeft (Right y) = assertFailure $ "Expected Left, received Right: " ++ show y

hassertLeft :: (Show a) => Either e a -> PropertyT IO e
hassertLeft (Left x) = pure x
hassertLeft (Right y) = do
  annotate $ "Expected Left, received Right: " ++ show y
  failure

assertRight :: (Show e) => Either e a -> IO a
assertRight (Left x) = assertFailure $ "Expected Left, received Right: " ++ show x
assertRight (Right y) = pure y

hassertRight :: (Show e) => Either e a -> PropertyT IO a
hassertRight (Left x) = do
  annotate $ "Expected Right, received Left: " ++ show x
  failure
hassertRight (Right y) = pure y

testProp :: TestName -> PropertyName -> PropertyT IO () -> TestTree
testProp n d = TH.testPropertyNamed n d . H.property
