{-# LANGUAGE Arrows #-}

module RMCA.Auxiliary.Yampa where

import FRP.Yampa
import Data.Maybe
import RMCA.Auxiliary.Misc

-- | = Yampa

countTo :: (Integral b) => b -> SF (Event a) (Event b)
countTo n = count >>^ filterE (> n)

-- | 'stepBack' contains its previous argument as its output. Because it's hard to define it at time 0, it's wrapped up in a 'Maybe'.
stepBack :: SF a (Maybe a)
stepBack = sscan f (Nothing, Nothing) >>^ snd
  where f :: (Maybe a, Maybe a) -> a -> (Maybe a, Maybe a)
        f (Nothing,_) x' = (Just x', Nothing)
        f (Just x, _) x' = (Just x', Just x)

-- | Like 'stepBack' but the output value is always defined and is equal to the input at time 0.
stepBack' :: SF a a
stepBack' = proc x -> do
  x' <- stepBack -< x
  returnA -< fromMaybe x x'

-- | Throws an 'Event' when the incoming signal change. The 'Event' is tagged with the new value.
onChange :: (Eq a) => SF a (Event a)
onChange = proc x -> do
  x' <- stepBack -< x
  let makeEvent x x'
        | isNothing x' = NoEvent
        | otherwise = let x'' = fromJust x' in
            if x'' == x then NoEvent else Event x
  returnA -< makeEvent x x'

-- | Similar to 'onChange' but contains its initial value in the first
-- event.
onChange' :: (Eq a) => SF a (Event a)
onChange' = proc x -> do
  x' <- stepBack -< x
  -- If it's the first value, throw an Event, else behave like onChange.
  let makeEvent x x'
        | isNothing x' = Event x
        | otherwise = let x'' = fromJust x' in
            if x'' == x then NoEvent else Event x
  returnA -< makeEvent x x'

-- | Integrates some variable modulo something.
integralMod :: (Real a, VectorSpace a s) => a -> SF a a
integralMod x = intMod' 0
  where intMod' x0 = switch (intMod'' x0) (\y -> intMod' (y - x))
        intMod'' x0 =  proc t -> do
          it <- (+ x0) ^<< integral -< t
          es <- edgeBy (\_ y -> maybeIf (y > x) $> y) 0 -< it
          returnA -< (it,es)



-- | Generates a sine function whose period is given as a varying input.
varFreqSine :: SF DTime Double
varFreqSine = sin ^<< (2*pi*) ^<< integralMod 1 <<^ (1/)

-- | Generates an 'Event' with a regular period, which is given as an input to the signal function.
repeatedlyS :: a -> SF DTime (Event a)
repeatedlyS x = edgeBy (\a b -> maybeIf (a * b < 0) $> x) 0
                <<< varFreqSine <<^ (2*)

repeatedlyS' :: a -> SF DTime (Event a)
repeatedlyS' x = (repeatedlyS x &&& now x) >>> arr (uncurry lMerge)

-- |
-- = Curry and uncurry functions
