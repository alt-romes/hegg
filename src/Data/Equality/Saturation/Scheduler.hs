{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-} -- Scheduler
{-# LANGUAGE TypeFamilies #-}
module Data.Equality.Saturation.Scheduler where

import qualified Data.IntMap as IM
import Data.Equality.Matching

class Scheduler s where
    type Stat s

    -- | Scheduler: update stats
    updateStats :: Int                -- ^ Iteration we're in
                -> Int                -- ^ Index of rewrite rule we're updating
                -> Maybe (Stat s)     -- ^ Current stat for this rewrite rule (we already got it so no point in doing a lookup again)
                -> IM.IntMap (Stat s) -- ^ The current stats map
                -> [Match]            -- ^ The list of matches resulting from matching this rewrite rule
                -> IM.IntMap (Stat s) -- ^ The updated map with new stats

    -- Decide whether to apply a matched rule based on its stats and current iteration
    isBanned :: Int -- ^ Iteration we're in
             -> Stat s -- ^ Stats for the rewrite rule
             -> Bool -- ^ Whether the rule should be applied or not

data BackoffScheduler
instance Scheduler BackoffScheduler where
    type Stat BackoffScheduler = BoSchStat

    -- | Backoff scheduler: update stats
    -- updateStats :: Int            -- ^ Iteration we're in
    --             -> Int            -- ^ Index of rewrite rule we're updating
    --             -> Maybe BoSchStat     -- ^ Current stat for this rewrite rule (we already got it so no point in doing a lookup again)
    --             -> IM.IntMap BoSchStat -- ^ The map to update
    --             -> [Match]        -- ^ The list of matches resulting from matching this rewrite rule
    --             -> IM.IntMap BoSchStat -- ^ The updated map
    updateStats i rw currentStat stats matches =

        if total_len > threshold

          then
            IM.alter updateBans rw stats

          else
            stats

        where

          -- TODO: Overall difficult, and buggy at the moment.
          total_len = sum (map (length . matchSubst) matches)

          defaultMatchLimit = 140 -- they're using 1000...
          defaultBanLength  = 10

          bannedN = case currentStat of
                      Nothing -> 0;
                      Just (timesBanned -> n) -> n

          threshold = defaultMatchLimit * (2^bannedN)

          ban_length = defaultBanLength * (2^bannedN)

          updateBans = \case
            Nothing -> Just (BSS (i + ban_length) 1)
            Just (BSS _ n)  -> Just (BSS (i + ban_length) (n+1))

    i `isBanned` s = i < bannedUntil s


data BoSchStat = BSS { bannedUntil :: {-# UNPACK #-} !Int
                     , timesBanned :: {-# UNPACK #-} !Int
                     } deriving Show
