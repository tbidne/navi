module Unit.Navi.Services.Custom.Multiple.Toml (tests) where

import DBus.Notify (UrgencyLevel (Critical))
import Data.Text qualified as T
import Hedgehog qualified as H
import Hedgehog.Gen qualified as G
import Navi.Data.NaviNote (NaviNote (MkNaviNote), Timeout (Seconds))
import Navi.Event.Toml
  ( ErrorNoteToml
      ( ErrNoteAllowRepeatsToml,
        ErrNoteNoRepeatsToml,
        NoErrNoteToml
      ),
    MultiRepeatEventToml (MultiAllowRepeatsToml, MultiNoRepeatsToml),
  )
import Navi.Services.Custom.Multiple.Toml
  ( MultipleToml,
    TriggerNoteToml
      ( MkTriggerNoteToml,
        note,
        trigger
      ),
  )
import Unit.Prelude
import Unit.Utils qualified as Utils

tests :: TestTree
tests =
  testGroup
    "Navi.Services.Custom.Multiple.Toml"
    [ parsesCmd,
      parsesNotes,
      repeatEventTests,
      errorNoteTests,
      cmdResultTests
    ]

parsesCmd :: TestTree
parsesCmd =
  parsesExpected
    "Parses command"
    txt
    "  some multiline\n  command-1\n"
    (view #command)
  where
    txt =
      T.unlines
        [ "command = \"\"\"",
          "  some multiline",
          "  command-1",
          "\"\"\"",
          "[[trigger-note]]",
          "trigger = \"value\"",
          "summary = \"a summary\""
        ]

parsesNotes :: TestTree
parsesNotes =
  parsesExpected
    "Parses notes"
    txt
    (firstNote :| [secondNote])
    (view #triggerNotes)
  where
    txt =
      T.unlines
        [ "command = \"cmd\"",
          "[[trigger-note]]",
          "trigger = \"first val\"",
          "summary = \"first summary\"",
          "body = \"first body\"",
          "urgency = \"critical\"",
          "timeout = 5",
          "[[trigger-note]]",
          "trigger = \"second val\"",
          "summary = \"second summary\""
        ]
    firstNote =
      MkTriggerNoteToml
        { trigger = "first val",
          note = MkNaviNote "first summary" (Just "first body") (Just Critical) (Just $ Seconds 5)
        }
    secondNote =
      MkTriggerNoteToml
        { trigger = "second val",
          note = MkNaviNote "second summary" Nothing Nothing Nothing
        }

repeatEventTests :: TestTree
repeatEventTests =
  testGroup
    "Parses repeat event"
    [ parsesRepeatEvent "off" "false" MultiNoRepeatsToml,
      parsesRepeatEvent "on" "true" MultiAllowRepeatsToml,
      parsesExpected "<none>" txt Nothing (view #repeatEventCfg)
    ]
  where
    txt =
      T.unlines
        [ "command = \"cmd\"",
          "[[trigger-note]]",
          "trigger = \"value\"",
          "summary = \"a summary\""
        ]

parsesRepeatEvent :: String -> Text -> MultiRepeatEventToml Text -> TestTree
parsesRepeatEvent desc flag ret =
  parsesExpected
    desc
    txt
    (Just ret)
    (view #repeatEventCfg)
  where
    txt =
      T.unlines
        [ "command = \"cmd\"",
          "repeat-events = " <> flag,
          "[[trigger-note]]",
          "trigger = \"value\"",
          "summary = \"a summary\""
        ]

errorNoteTests :: TestTree
errorNoteTests =
  testGroup
    "Parses error note"
    [ parsesErrorEvent "off" "none" NoErrNoteToml,
      parsesErrorEvent "on" "repeats" ErrNoteAllowRepeatsToml,
      parsesErrorEvent "on" "no-repeats" ErrNoteNoRepeatsToml,
      parsesExpected "<none>" txt Nothing (view #errEventCfg)
    ]
  where
    txt =
      T.unlines
        [ "command = \"cmd\"",
          "[[trigger-note]]",
          "trigger = \"value\"",
          "summary = \"a summary\""
        ]

parsesErrorEvent :: String -> Text -> ErrorNoteToml -> TestTree
parsesErrorEvent desc flag ret =
  parsesExpected
    desc
    txt
    (Just ret)
    (view #errEventCfg)
  where
    txt =
      T.unlines
        [ "command = \"cmd\"",
          "error-events = \"" <> flag <> "\"",
          "[[trigger-note]]",
          "trigger = \"value\"",
          "summary = \"a summary\""
        ]

cmdResultTests :: TestTree
cmdResultTests =
  testGroup
    "command-result"
    [ testCommandResultSuccess0,
      cmdResult1Tests,
      cmdResult2Tests,
      cmdResult3Tests
    ]

testCommandResultSuccess0 :: TestTree
testCommandResultSuccess0 = assertDecode @MultipleToml desc txt $ \toml -> do
  case toml ^. #parser of
    Nothing -> pure ()
    Just _ -> assertFailure "Expected no explicit parser"
  where
    desc = "No 'command-result' uses default parser"
    txt =
      T.unlines
        [ "command = \"cmd\"",
          "[[trigger-note]]",
          "trigger = \"value\"",
          "summary = \"a summary\""
        ]

cmdResult1Tests :: TestTree
cmdResult1Tests =
  testGroup
    "1 element"
    [ testCommandResultSuccess1,
      testCommandResultFailure1
    ]

testCommandResultSuccess1 :: TestTree
testCommandResultSuccess1 = testCase desc $ do
  -- no parens
  let cfg1 = mkTxt "trigger"
  toml1 <- assertRight $ decode @MultipleToml cfg1

  parser1 <- case toml1 ^? (#parser %? #unCommandResultParserToml) of
    Nothing -> assertFailure "Expected an explicit parser"
    Just p -> pure $ p "name" ^. #unCommandResultParser

  -- always succeeds
  s1 <- assertRight $ parser1 "foo"
  "foo" @=? s1 ^. #result
  Nothing @=? s1 ^. #output
  Nothing @=? s1 ^. #pollInterval

  -- parens
  let cfg2 = mkTxt "(trigger)"
  toml2 <- assertRight $ decode @MultipleToml cfg2

  parser2 <- case toml2 ^? (#parser %? #unCommandResultParserToml) of
    Nothing -> assertFailure "Expected an explicit parser"
    Just p -> pure $ p "name" ^. #unCommandResultParser

  -- always succeeds
  s2 <- assertRight $ parser2 "foo"
  "foo" @=? s2 ^. #result
  Nothing @=? s2 ^. #output
  Nothing @=? s2 ^. #pollInterval
  where
    desc = "command-result single success"
    mkTxt txt =
      T.unlines
        [ "command = \"cmd\"",
          "command-result = \"" <> txt <> "\"",
          "[[trigger-note]]",
          "trigger = \"value\"",
          "summary = \"a summary\""
        ]

testCommandResultFailure1 :: TestTree
testCommandResultFailure1 = testProp "testCommandResultFailure1" desc $ do
  (core, cr) <- forAll g

  let cfg =
        T.unlines
          [ "command = \"cmd\"",
            "command-result = \"" <> cr <> "\"",
            "[[trigger-note]]",
            "trigger = \"value\"",
            "summary = \"a summary\""
          ]

  case decode @MultipleToml cfg of
    Left e -> do
      annotateShow e
      let eTxt = packText $ displayException e
      H.assert
        $ "Decode error at '.command-result': A single command result can only be 'trigger', found: "
        `T.isPrefixOf` eTxt
      H.assert $ core `T.isInfixOf` eTxt
    Right toml -> do
      annotate $ "Unexpected success: " ++ show toml
      failure
  where
    desc = "command-result single failure"

    g :: Gen (Text, Text)
    g = do
      txt <- G.element ["output", "poll-interval"]

      addParens <- G.enumBounded
      let final =
            if addParens
              then "(" <> txt <> ")"
              else txt

      pure (txt, final)

cmdResult2Tests :: TestTree
cmdResult2Tests =
  testGroup
    "2 elements"
    [ testCommandResultSuccess2,
      testCommandResultFailure2
    ]

testCommandResultSuccess2 :: TestTree
testCommandResultSuccess2 = testProp "testCommandResultSuccess2" desc $ do
  cr <- forAll G.enumBounded

  let crTxt = Utils.mkCrs2Config cr
      input = Utils.mkCrs2Input cr
      assertOutput = Utils.assertCrs2Output cr
      cfg =
        T.unlines
          [ "command = \"cmd\"",
            "command-result = \"" <> crTxt <> "\"",
            "[[trigger-note]]",
            "trigger = \"value\"",
            "summary = \"a summary\""
          ]

  annotateShow crTxt
  annotateShow input

  parser <- case decode @MultipleToml cfg of
    Left err -> do
      annotate $ "Parsing config fails: " <> show err
      failure
    Right toml -> do
      case toml ^? (#parser %? #unCommandResultParserToml) of
        Nothing -> do
          annotate "Expected an explicit parser"
          failure
        Just p -> pure $ p "name" ^. #unCommandResultParser

  e1 <- hassertLeft $ parser "foo"
  annotateShow e1
  let e1Txt = packText $ displayException e1
  H.assert $ "name: Failed to parse " `T.isPrefixOf` e1Txt
  H.assert $ crTxt `T.isInfixOf` e1Txt
  H.assert $ ". Expected '(', received: foo" `T.isSuffixOf` e1Txt

  e2 <- hassertLeft $ parser "(foo)"
  annotateShow e2
  let e2Txt = packText $ displayException e2
  H.assert $ "name: Failed to parse " `T.isPrefixOf` e2Txt
  H.assert $ crTxt `T.isInfixOf` e1Txt
  H.assert $ "Expected 2-tuple, found 1 element: (foo)" `T.isSuffixOf` e2Txt

  s1 <- hassertRight $ parser input
  assertOutput s1
  where
    desc = "command-result 2-tuple success"

testCommandResultFailure2 :: TestTree
testCommandResultFailure2 = testProp "testCommandResultFailure2" desc $ do
  cr <- forAll G.enumBounded

  let crTxt = Utils.mkCrf2Config cr
      cfg =
        T.unlines
          [ "command = \"cmd\"",
            "command-result = \"" <> crTxt <> "\"",
            "[[trigger-note]]",
            "trigger = \"value\"",
            "summary = \"a summary\""
          ]

  annotateShow crTxt

  case decode @MultipleToml cfg of
    Left e -> do
      annotateShow e
      let eTxt = packText $ displayException e
      H.assert $ "Decode error at '.command-result': " `T.isPrefixOf` eTxt
      H.assert $ crTxt `T.isInfixOf` eTxt
    Right toml -> do
      annotate $ "Unexpected success: " ++ show toml
      failure
  where
    desc = "command-result 2-tuple failure"

cmdResult3Tests :: TestTree
cmdResult3Tests =
  testGroup
    "3 elements"
    [ testCommandResultSuccess3,
      testCommandResultFailure3
    ]

testCommandResultSuccess3 :: TestTree
testCommandResultSuccess3 = testProp "testCommandResultSuccess3" desc $ do
  cr <- forAll G.enumBounded

  let crTxt = Utils.mkCrs3Config cr
      input = Utils.mkCrs3Input cr
      cfg =
        T.unlines
          [ "command = \"cmd\"",
            "command-result = \"" <> crTxt <> "\"",
            "[[trigger-note]]",
            "trigger = \"value\"",
            "summary = \"a summary\""
          ]

  annotateShow crTxt
  annotateShow input

  parser <- case decode @MultipleToml cfg of
    Left err -> do
      annotate $ "Parsing config fails: " <> show err
      failure
    Right toml -> do
      case toml ^? (#parser %? #unCommandResultParserToml) of
        Nothing -> do
          annotate "Expected an explicit parser"
          failure
        Just p -> pure $ p "name" ^. #unCommandResultParser

  e1 <- hassertLeft $ parser "foo"
  annotateShow e1
  let e1Txt = packText $ displayException e1
  H.assert $ "name: Failed to parse " `T.isPrefixOf` e1Txt
  H.assert $ crTxt `T.isInfixOf` e1Txt
  H.assert $ ". Expected '(', received: foo" `T.isSuffixOf` e1Txt

  e2 <- hassertLeft $ parser "(foo)"
  annotateShow e2
  let e2Txt = packText $ displayException e2
  H.assert $ "name: Failed to parse " `T.isPrefixOf` e2Txt
  H.assert $ crTxt `T.isInfixOf` e1Txt
  H.assert $ "Expected 3-tuple, found 1 element: (foo)" `T.isSuffixOf` e2Txt

  e3 <- hassertLeft $ parser "(foo, bar)"
  annotateShow e3
  let e3Txt = packText $ displayException e3
  H.assert $ "name: Failed to parse " `T.isPrefixOf` e3Txt
  H.assert $ crTxt `T.isInfixOf` e1Txt
  H.assert $ "Expected 3-tuple, found 2 elements: (foo, bar)" `T.isSuffixOf` e3Txt

  s1 <- hassertRight $ parser input
  Utils.assertCrs3Output s1
  where
    desc = "command-result 3-tuple success"

testCommandResultFailure3 :: TestTree
testCommandResultFailure3 = testProp "testCommandResultFailure3" desc $ do
  cr <- forAll G.enumBounded

  let crTxt = Utils.mkCrf3Config cr
      cfg =
        T.unlines
          [ "command = \"cmd\"",
            "command-result = \"" <> crTxt <> "\"",
            "[[trigger-note]]",
            "trigger = \"value\"",
            "summary = \"a summary\""
          ]

  annotateShow crTxt

  case decode @MultipleToml cfg of
    Left e -> do
      annotateShow e
      let eTxt = packText $ displayException e
      H.assert $ "Decode error at '.command-result': " `T.isPrefixOf` eTxt
      H.assert $ crTxt `T.isInfixOf` eTxt
    Right toml -> do
      annotate $ "Unexpected success: " ++ show toml
      failure
  where
    desc = "command-result 3-tuple failure"

parsesExpected ::
  (Eq a, Show a) =>
  String ->
  Text ->
  a ->
  (MultipleToml -> a) ->
  TestTree
parsesExpected = decodeExpected
