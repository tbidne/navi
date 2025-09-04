module Unit.Utils
  ( -- * Generators

    -- ** Command Result
    CommandResultSuccess2 (..),
    mkCrs2Config,
    mkCrs2Input,
    assertCrs2Output,
    CommandResultFailure2 (..),
    mkCrf2Config,
    CommandResultSuccess3 (..),
    mkCrs3Config,
    mkCrs3Input,
    assertCrs3Output,
    CommandResultFailure3 (..),
    mkCrf3Config,
  )
where

import Navi.Data.CommandResult (CommandResult)
import Unit.Prelude

data CommandResultSuccess2
  = TO
  | OT
  | TP
  | PT
  deriving stock (Bounded, Enum, Show)

mkCrs2Config :: CommandResultSuccess2 -> Text
mkCrs2Config TO = "(trigger, output)"
mkCrs2Config TP = "(trigger, poll-interval)"
mkCrs2Config OT = "(output, trigger)"
mkCrs2Config PT = "(poll-interval, trigger)"

mkCrs2Input :: CommandResultSuccess2 -> Text
mkCrs2Input TO = "(some result, some output)"
mkCrs2Input TP = "(some result, 5)"
mkCrs2Input OT = "(some output, some result)"
mkCrs2Input PT = "(5, some result)"

assertCrs2Output :: CommandResultSuccess2 -> CommandResult -> PropertyT IO ()
assertCrs2Output TO cr = do
  "some result" === cr ^. #result
  Just "some output" === cr ^. #output
  Nothing === cr ^. #pollInterval
assertCrs2Output OT cr = do
  "some result" === cr ^. #result
  Just "some output" === cr ^. #output
  Nothing === cr ^. #pollInterval
assertCrs2Output TP cr = do
  "some result" === cr ^. #result
  Just 5 === cr ^. #pollInterval
  Nothing === cr ^. #output
assertCrs2Output PT cr = do
  "some result" === cr ^. #result
  Just 5 === cr ^. #pollInterval
  Nothing === cr ^. #output

data CommandResultFailure2
  = TT
  | OO
  | OP
  | PO
  | PP
  deriving stock (Bounded, Enum, Show)

mkCrf2Config :: CommandResultFailure2 -> Text
mkCrf2Config TT = "(trigger, trigger)"
mkCrf2Config OO = "(output, output)"
mkCrf2Config OP = "(output, poll-interval)"
mkCrf2Config PO = "(poll-interval, output)"
mkCrf2Config PP = "(poll-interval, poll-interval)"

data CommandResultSuccess3
  = TOP
  | TPO
  | OTP
  | OPT
  | PTO
  | POT
  deriving stock (Bounded, Enum, Show)

mkCrs3Config :: CommandResultSuccess3 -> Text
mkCrs3Config TOP = "(trigger, output, poll-interval)"
mkCrs3Config TPO = "(trigger, poll-interval, output)"
mkCrs3Config OTP = "(output, trigger, poll-interval)"
mkCrs3Config OPT = "(output, poll-interval, trigger)"
mkCrs3Config PTO = "(poll-interval, trigger, output)"
mkCrs3Config POT = "(poll-interval, output, trigger)"

mkCrs3Input :: CommandResultSuccess3 -> Text
mkCrs3Input TOP = "(some result, some output, 5)"
mkCrs3Input TPO = "(some result, 5, some output)"
mkCrs3Input OTP = "(some output, some result, 5)"
mkCrs3Input OPT = "(some output, 5, some result)"
mkCrs3Input PTO = "(5, some result, some output)"
mkCrs3Input POT = "(5, some output, some result)"

assertCrs3Output :: CommandResult -> PropertyT IO ()
assertCrs3Output cr = do
  "some result" === cr ^. #result
  Just "some output" === cr ^. #output
  Just 5 === cr ^. #pollInterval

data CommandResultFailure3
  = TTT
  | TTO
  | TTP
  | TOT
  | TOO
  | TPT
  | TPP
  | OTT
  | OTO
  | OOT
  | OOO
  | OOP
  | OPO
  | OPP
  | PTT
  | PTP
  | POO
  | POP
  | PPT
  | PPO
  | PPP
  deriving stock (Bounded, Enum, Show)

mkCrf3Config :: CommandResultFailure3 -> Text
mkCrf3Config TTT = "(trigger, trigger, trigger)"
mkCrf3Config TTO = "(trigger, trigger, output)"
mkCrf3Config TTP = "(trigger, trigger, poll-interval)"
mkCrf3Config TOT = "(trigger, output, trigger)"
mkCrf3Config TOO = "(trigger, output, output)"
mkCrf3Config TPT = "(trigger, poll-interval, trigger)"
mkCrf3Config TPP = "(trigger, trigger, poll-interval)"
mkCrf3Config OTT = "(output, trigger, trigger)"
mkCrf3Config OTO = "(output, trigger, output)"
mkCrf3Config OOT = "(output, output, trigger)"
mkCrf3Config OOO = "(output, output, output)"
mkCrf3Config OOP = "(output, output, poll-interval)"
mkCrf3Config OPO = "(output, poll-interval, output)"
mkCrf3Config OPP = "(output, poll-interval, poll-interval)"
mkCrf3Config PTT = "(poll-interval, trigger, trigger)"
mkCrf3Config PTP = "(poll-interval, trigger, poll-interval)"
mkCrf3Config POO = "(poll-interval, output, output)"
mkCrf3Config POP = "(poll-interval, output, poll-interval)"
mkCrf3Config PPT = "(poll-interval, poll-interval, trigger)"
mkCrf3Config PPO = "(poll-interval, poll-interval, output)"
mkCrf3Config PPP = "(poll-interval, poll-interval, poll-interval)"
