{-# LANGUAGE Arrows #-}

module RMCA.Auxiliary.Auxiliary where

import Data.Maybe
import FRP.Yampa

-- stepBack contains its previous argument as its output. Because it's
-- hard to define it at time 0, it's wrapped up in a Maybe.
stepBack :: SF a (Maybe a)
stepBack = sscan f (Nothing, Nothing) >>^ snd
  where f :: (Maybe a, Maybe a) -> a -> (Maybe a, Maybe a)
        f (Nothing,_) x' = (Just x', Nothing)
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
  returnA -< makeEvent x x'

-- Similar to onChange but contains its initial value in the first
-- event.
onChange' :: (Eq a) => SF a (Event a)
onChange' = proc x -> do
  x' <- stepBack -< x
  returnA -< makeEvent x x'

makeEvent :: (Eq a) => a -> Maybe a -> Event a
makeEvent x x'
  | isNothing x' = Event x
  | otherwise = let x'' = fromJust x' in
                  if x'' == x then NoEvent else Event x

discard :: a -> ()
discard _ = ()

bound :: (Ord a) => (a, a) -> a -> a
bound (min, max) x
  | x < min = min
  | x > max = max
  | otherwise = x
