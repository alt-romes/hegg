{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-|

Definition of 'Scheduler' as a way to control application of rewrite rules.

The 'BackoffScheduler' is a scheduler which implements exponential rule backoff
and is used by default in 'Data.Equality.Saturation.equalitySaturation'

-}
module Data.Equality.Saturation.Scheduler
    ( Scheduler(..)

    , BackoffScheduler(..), defaultBackoffScheduler

    , TracingScheduler(..)
    ) where

import qualified Data.IntMap.Strict as IM
import Data.Equality.Matching
import Data.Equality.Matching.Database (sizeSubst)
import Data.Equality.Saturation.Rewrites (Rewrite)
import qualified Debug.Trace as Debug
import Data.Coerce (coerce)

-- | A 'Scheduler' determines whether a certain rewrite rule is banned from
-- being used based on statistics it defines and collects on applied rewrite
-- rules.
class Scheduler l s where
    data Stat l s

    -- | Scheduler: update stats
    updateStats :: s                  -- ^ The scheduler itself
                -> Int                -- ^ Iteration we're in
                -> Int                -- ^ Index of rewrite rule we're updating
                -> Rewrite a l          -- ^ The rewrite rule that matched
                -> Maybe (Stat l s)     -- ^ Current stat for this rewrite rule (we already got it so no point in doing a lookup again)
                -> IM.IntMap (Stat l s) -- ^ The current stats map
                -> [Match]            -- ^ The list of matches resulting from matching this rewrite rule
                -> IM.IntMap (Stat l s) -- ^ The updated map with new stats

    -- Decide whether to apply a matched rule based on its stats and current iteration
    isBanned :: Int -- ^ Iteration we're in
             -> Stat l s -- ^ Stats for the rewrite rule
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

instance Scheduler l BackoffScheduler where
    data Stat l BackoffScheduler =
      BSS { bannedUntil :: {-# UNPACK #-} !Int
          , timesBanned :: {-# UNPACK #-} !Int
          } deriving Show

    updateStats bos i rw_id _ currentStat stats matches =

        if total_len > threshold

          then
            IM.alter updateBans rw_id stats

          else
            stats

        where

          -- TODO: Overall difficult, and buggy at the moment.
          total_len = sum (map (sizeSubst . matchSubst) matches)

          bannedN = case currentStat of
                      Nothing -> 0;
                      Just (timesBanned -> n) -> n

          threshold = matchLimit bos * (2^bannedN)

          ban_length = banLength bos * (2^bannedN)

          updateBans = \case
            Nothing -> Just (BSS (i + ban_length) 1)
            Just (BSS _ n)  -> Just (BSS (i + ban_length) (n+1))

    isBanned i s = i < bannedUntil s

--------------------------------------------------------------------------------
-- * Tracing

-- | A Scheduler wrapping another scheduler but additionally traces (with
-- Debug.Trace) all rules which are applied.
--
-- This is helpful to debug loopy sets of rewrite rules
--
-- === Example
-- @
-- 'runEqualitySaturation' rewrites ('TracingScheduler' 'defaultBackoffScheduler')
-- @
newtype TracingScheduler a = TracingScheduler a

-- | This instance wraps the underlying scheduler, making sure
instance ((forall a. Show a => Show (l a)), Scheduler l s) => Scheduler l (TracingScheduler s) where
  newtype Stat l (TracingScheduler s) = TracingStat (Stat l s)

  updateStats (TracingScheduler sch) i rw_id rw currentStat stats matches =
    Debug.trace ("Rule matched: " ++ show rw) $ coerce $
    updateStats @l @s sch i rw_id rw (coerce currentStat) (coerce stats) matches

  isBanned i (TracingStat s) = isBanned @l @s i s
