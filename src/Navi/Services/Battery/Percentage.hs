-- | This module provides a service for alerts related to battery levels.
module Navi.Services.Battery.Percentage
  ( BatteryPercentageToml,
    toEvent,
  )
where

import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as Map
import Navi.Data.NaviNote (NaviNote (MkNaviNote))
import Navi.Data.PollInterval (PollInterval (MkPollInterval))
import Navi.Event.Toml qualified as EventToml
import Navi.Event.Types
  ( AnyEvent (MkAnyEvent),
    ErrorNote,
    Event
      ( MkEvent,
        errorNote,
        name,
        pollInterval,
        raiseAlert,
        repeatEvent,
        serviceType
      ),
    RepeatEvent,
  )
import Navi.Prelude
import Navi.Services.Battery.Percentage.Toml
  ( BatteryPercentageNoteToml,
    BatteryPercentageToml,
    PercentageData (PercentageExact, PercentageRange),
  )
import Navi.Services.Types (ServiceType (BatteryPercentage))
import Pythia.Services.Battery
  ( Battery,
    BatteryApp,
    BatteryStatus (Discharging),
  )

-- | Transforms toml configuration data into an 'AnyEvent'.
toEvent :: (MonadIORef m) => BatteryPercentageToml -> m AnyEvent
toEvent toml = do
  repeatEvent <- EventToml.mMultiRepeatEventTomlToVal PercentageExact $ toml ^. #repeatEvent
  errorNote <- EventToml.mErrorNoteTomlToVal $ toml ^. #errorNote
  let evt = mkBatteryEvent percentNoteList app pi repeatEvent errorNote
  pure $ MkAnyEvent evt
  where
    percentNoteList = tomlToNote <$> toml ^. #alerts
    app = toml ^. #app
    pi = fromMaybe (MkPollInterval 30) (toml ^. #pollInterval)
{-# INLINEABLE toEvent #-}

tomlToNote :: BatteryPercentageNoteToml -> (PercentageData, NaviNote)
tomlToNote toml =
  ( percentage,
    MkNaviNote
      summary
      Nothing
      (toml ^. #urgency)
      (toml ^. #mTimeout)
  )
  where
    percentage = toml ^. #percentage
    summary = "Battery Percentage"

-- NOTE: A battery percentage event has result Battery and trigger
-- PercentageData. There are two reasons why these are not the same type.
--
-- 1. We should only raise an alert when the battery status is discharging,
--    hence we need the status. But we do not need to save this in the
--    trigger for repeat-events, as we can assume only discharging percentages
--    were sent.
--
-- 2. We want to store PercentageData, not Percentage. A battery reading
--    corresponds to exactly one percentage, hence Percentage. But the
--    trigger is based on PercentageData, therefore this is what we need to
--    store for correct block-repeat semantics.
--
-- Suppose we have a battery percentage range r = [0, 10] and querying the
-- service yields {discharging, 8}. We will send off a notif, since it is
-- in range.
--
-- Now suppose we read {discharging, 6}. Should we send off a notif? It depends
-- entirely on how repeat-events was configured.
--
--   - If repeats are allowed for this range (either repeat-events = true or
--     repeat-events = [..., 0 ,...]), then we need to know 6 corresponds to an
--     allowed range, hence storing the range [0, 10], not the last value 8.
--
--   - If repeats are not allowed, then we still need to know that 6
--     corresponds to the previous range [0, 10], and block it. Relying on the
--     actual value (6) would produce the wrong result.

mkBatteryEvent ::
  NonEmpty (PercentageData, NaviNote) ->
  BatteryApp ->
  PollInterval ->
  RepeatEvent PercentageData ->
  ErrorNote ->
  Event Battery PercentageData
mkBatteryEvent percentNoteList batteryProgram pollInterval repeatEvent errorNote =
  MkEvent
    { name = "battery-percentage",
      serviceType = BatteryPercentage batteryProgram,
      pollInterval,
      raiseAlert = \b ->
        -- Though the trigger is based on the range, we want to display the
        -- actual percentage i.e. param b.
        set' (_Just % _2 % #body) (Just $ display $ b ^. #percentage)
          $ lookupPercent percentNoteMap b,
      repeatEvent,
      errorNote
    }
  where
    percentNoteMap = Map.fromList $ NE.toList percentNoteList

lookupPercent :: Map PercentageData NaviNote -> Battery -> Maybe (PercentageData, NaviNote)
lookupPercent percentNoteMap state = case state ^. #status of
  -- lookupLE so we can attempt to find ranges as well. Note that this can
  -- behave unexpectedly when ranges overlap. E.g. suppose our map contains
  -- keys:
  --
  --   - [40, 60]
  --   - 50
  --
  -- And we receive 55. Arguably the [40, 60] range _should_ apply here, but
  -- the lookup will find exact 50 first, then fail the equality check.
  -- Handling this would be complicated for little gain, so we make a note
  -- of it here, and advise that percentages should not overlap.
  Discharging -> case Map.lookupLE (PercentageExact p) percentNoteMap of
    Nothing -> Nothing
    -- Found exact key, need to check equality.
    Just (r@(PercentageExact q), note)
      -- Exact match, send off note.
      | p == q -> Just (r, note)
      -- Not a match, do nothing.
      | otherwise -> Nothing
    -- Found a range, need to check bounds.
    Just (r@(PercentageRange low high), note)
      | p >= low && p < high -> Just (r, note)
      | otherwise -> Nothing
  _ -> Nothing
  where
    p = state ^. #percentage
