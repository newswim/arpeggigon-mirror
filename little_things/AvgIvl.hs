{-# LANGUAGE Arrows #-}

module AvgIvl ( avgIvl
              ) where

import FRP.Yampa

import Debug.Trace

ivlNum :: Int
ivlNum = 3

maxTime :: DTime
maxTime = 5

infinity :: (Fractional a) => a
infinity = 1/0

-- Outputs the average time between ivlNum of the last events. Goes to
-- infinity if less than ivlNum events have occured or if no event has
-- occured in maxTime.
avgIvl :: SF (Event a) DTime
avgIvl = switch (constant infinity &&& constant (Event [])) avgIvl'
  where
    avgIvl' l = switch avgIvl'' (avgIvl')
      where  avgIvl'' :: SF (Event a) (DTime, Event [DTime])
             avgIvl'' = proc e -> do
               e' <- notYet -< e
               t <- localTime -< ()
               tooLate <- after maxTime [] -< ()
               let timeList = (e' `tag` (appDTime ivlNum t l)) `lMerge` tooLate
               returnA -< (avgS ivlNum l, timeList)

appDTime :: Int -> Time -> [DTime] -> [DTime]
appDTime _ _ [] = [0]
appDTime n t l = t:(take (n-1) l)

avgS :: (Fractional a) => Int -> [a] -> a
avgS n l
  | length l /= n = infinity
  | otherwise = foldl (+) 0 l / fromIntegral n
