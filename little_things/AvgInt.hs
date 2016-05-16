{-# LANGUAGE Arrows #-}

module AvgInt ( avgInt
              ) where

import FRP.Yampa

intNum :: Int
intNum = 3

maxTime :: DTime
maxTime = 10

infinity :: (Fractional a) => a
infinity = 1/0

-- Outputs the average time between intNum of the last events. Goes to
-- infinity if less than intNum events have occured or if no event has
-- occured in maxTime.
avgInt :: SF (Event a) DTime
avgInt = avgInt' [] `switch` ((>>^ fst) . avgInt')
  where avgInt' :: [DTime] -> SF (Event a) (DTime, Event [DTime])
        avgInt' l = proc e -> do
          t <- localTime -< ()
          tooLate <- after maxTime [] -< ()
          let timeList = (e `tag` (appDTime intNum t l)) `lMerge` tooLate
          returnA -< (avgS intNum l, timeList)

appDTime :: Int -> Time -> [DTime] -> [DTime]
appDTime _ _ [] = [0]
appDTime n t l = (t - head l):(take (n-1) l)

avgS :: (Fractional a) => Int -> [a] -> a
avgS n l
  | length l /= n = infinity
  | otherwise = foldl (+) 0 l / fromIntegral n
