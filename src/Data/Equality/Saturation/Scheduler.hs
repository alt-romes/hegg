{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-} -- Scheduler
{-# LANGUAGE TypeFamilies #-}
{-|

Definition of 'Scheduler' as a way to control application of rewrite rules.

The 'BackoffScheduler' is a scheduler which implements exponential rule backoff
and is used by default in 'Data.Equality.Saturation.equalitySaturation'

-}
module Data.Equality.Saturation.Scheduler
    ( Scheduler(..), BackoffScheduler(..), defaultBackoffScheduler
    ) where

import qualified Data.IntMap.Strict as IM
import Data.Equality.Matching

-- | A 'Scheduler' determines whether a certain rewrite rule is banned from
-- being used based on statistics it defines and collects on applied rewrite
-- rules.
class Scheduler s where
    data Stat s

    -- | Scheduler: update stats
    updateStats :: s                  -- ^ The scheduler itself
                -> Int                -- ^ Iteration we're in
                -> Int                -- ^ Index of rewrite rule we're updating
                -> Maybe (Stat s)     -- ^ Current stat for this rewrite rule (we already got it so no point in doing a lookup again)
                -> IM.IntMap (Stat s) -- ^ The current stats map
                -> [Match]            -- ^ The list of matches resulting from matching this rewrite rule
                -> IM.IntMap (Stat s) -- ^ The updated map with new stats

    -- Decide whether to apply a matched rule based on its stats and current iteration
    isBanned :: Int -- ^ Iteration we're in
             -> Stat s -- ^ Stats for the rewrite rule
             -> Bool -- ^ Whether the rule should be applied or not

-- | A 'Scheduler' that implements exponentional rule backoff.
--
-- For each rewrite, there exists a configurable initial match limit. If a rewrite
-- search yield more than this limit, then we ban this rule for number of
-- iterations, double its limit, and double the time it will be banned next time.
--
-- This seems effective at preventing explosive rules like associativity from
-- taking an unfair amount of resources.
--
-- Originaly in [egg](https://docs.rs/egg/0.6.0/egg/struct.BackoffScheduler.html)
data BackoffScheduler = BackoffScheduler
  { matchLimit :: {-# UNPACK #-} !Int
  , banLength  :: {-# UNPACK #-} !Int }

-- | The default 'BackoffScheduler'.
-- 
-- The match limit is set to @1000@ and the ban length is set to @10@.
defaultBackoffScheduler :: BackoffScheduler
defaultBackoffScheduler = BackoffScheduler 1000 10

instance Scheduler BackoffScheduler where
    data Stat BackoffScheduler =
      BSS { bannedUntil :: {-# UNPACK #-} !Int
          , timesBanned :: {-# UNPACK #-} !Int
          } deriving Show

    updateStats bos i rw currentStat stats matches =

        if total_len > threshold

          then
            IM.alter updateBans rw stats

          else
            stats

        where

          -- TODO: Overall difficult, and buggy at the moment.
          total_len = sum (map (length . matchSubst) matches)

          bannedN = case currentStat of
                      Nothing -> 0;
                      Just (timesBanned -> n) -> n

          threshold = matchLimit bos * (2^bannedN)

          ban_length = banLength bos * (2^bannedN)

          updateBans = \case
            Nothing -> Just (BSS (i + ban_length) 1)
            Just (BSS _ n)  -> Just (BSS (i + ban_length) (n+1))

    isBanned i s = i < bannedUntil s

