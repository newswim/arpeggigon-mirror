{-# LANGUAGE Arrows #-}

module RMCA.Auxiliary.Yampa where

import FRP.Yampa
import Data.Maybe
import RMCA.Auxiliary.Misc
import Control.Monad

-- | = Yampa

countTo :: (Integral b) => b -> SF (Event a) (Event b)
countTo n = count >>^ filterE (== n)

-- | Synchonizes two event sources. An event on the first source will be delayed until an event occurs on the second.
--
-- Ex:
-- Event a => . . 1 . . . . 2 . . . 3 . . 4 . . . . . 5 . . 6 . . . . .
-- Event b => . a . . . b . . . c . . . . . . d . e . f . . . . . g . .
-- wairFor => . . . . . 1 . . . 2 . . . . . . 4 . . . 5 . . . . . 6 . .

-- A more direct approach, and without any use of *> to avoid depending
-- on applicatives.

waitForEvent :: SF (Event a, Event b) (Event a)
waitForEvent = sscanPrim procEvts NoEvent NoEvent
    where
        procEvts eaPrev (NoEvent,      NoEvent) = Just (eaPrev,  NoEvent)
        procEvts _      (ea@(Event _), NoEvent) = Just (ea,      NoEvent)
        procEvts eaPrev (NoEvent,      Event _) = Just (NoEvent, eaPrev)
        procEvts _      (ea@(Event _), Event _) = Just (NoEvent, ea)

{-
waitForEvent :: SF (Event a, Event b) (Event a)
waitForEvent = proc (ea,eb) -> do
  em <- arr $ uncurry $ mapMerge Left Right (\_ b -> Right b) -< (ea,eb)
  hob <- dAccumHoldBy accumulator NoEvent -< em
  returnA -< eb *> (ea `lMerge` hob)
  where accumulator :: Event a -> Either a b -> Event a
        accumulator _ (Left a) = Event a
        accumulator _ (Right _) = NoEvent
        --accumulator _ (Right b) =
-}

{-
waitForEvent :: SF (Event b, Event a) (Event b)
waitForEvent = proc (eb,ea) -> do
  rec
    es' <- iPre NoEvent -< es
    es <- rSwitch waitAux -< ((eb,ea),es' `tag` waitAux)
  returnA -< es
  where waitAux = proc (eb,ea) -> do
          --ea' <- (if b then notYet else identity) -< ea
          eb' <- accumHoldBy (\_ b -> Event b) NoEvent -< eb
          returnA -< ea *> eb'
-}
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
          es <- edgeBy (\_ y -> if y > x then Just y else Nothing) 0 -< it
          returnA -< (it,es)



-- | Generates a sine function whose period is given as a varying input.
varFreqSine :: SF DTime Double
varFreqSine = sin ^<< (2*pi*) ^<< integralMod 1 <<^ (1/)

-- | Generates an 'Event' with a regular period, which is given as an input to the signal function.
repeatedlyS :: a -> SF DTime (Event a)
repeatedlyS x = edgeBy (\a b -> if a * b < 0 then Just x else Nothing) 0
                <<< varFreqSine <<^ (2*)

repeatedlyS' :: a -> SF DTime (Event a)
repeatedlyS' x = (repeatedlyS x &&& now x) >>> arr (uncurry lMerge)

-- |
-- = Curry and uncurry functions
