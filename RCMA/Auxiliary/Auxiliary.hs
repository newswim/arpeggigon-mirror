{-# LANGUAGE Arrows #-}

module RCMA.Auxiliary.Auxiliary where

import Data.Maybe
import FRP.Yampa

-- stepBack contains its previous argument as its output. Because it's
-- hard to define it at time 0, it's wrapped up in a Maybe.
stepBack :: SF a (Maybe a)
stepBack = sscan f (Nothing, Nothing) >>^ snd
  where f :: (Maybe a, Maybe a) -> a -> (Maybe a, Maybe a)
        f (Nothing,Nothing) x' = (Just x', Nothing)
        f (Just x, _) x' = (Just x', Just x)

-- Just like stepBack but the output value is always defined and is
-- equal to the input at time 0.
stepBack' :: SF a a
stepBack' = proc x -> do
  x' <- stepBack -< x
  returnA -< fromMaybe x x'

-- Throws an Event when the incoming signal change. The Event is
-- tagged with the new value.
onChange :: (Eq a) => SF a (Event a)
onChange = proc x -> do
  x' <- stepBack -< x
  let makeEvent x x'
        | isNothing x' = NoEvent
        | isJust x' = let x'' = fromJust x' in
            if x'' == x then NoEvent else Event x
  returnA -< makeEvent x x'

-- Similar to onChange but contains its initial value in the first
-- event.
onChange' :: (Eq a) => SF a (Event a)
onChange' = proc x -> do
  x' <- stepBack -< x
  -- If it's the first value, throw an Event, else behave like onChange.
  let makeEvent x x'
        | isNothing x' = Event x
        | isJust x' = let x'' = fromJust x' in
            if x'' == x then NoEvent else Event x
  returnA -< makeEvent x x'

discard :: a -> ()
discard _ = ()
