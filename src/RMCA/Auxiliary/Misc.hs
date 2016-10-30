module RMCA.Auxiliary.Misc where

import Data.Maybe
import FRP.Yampa

-- |= General functions


{-
-- | Reversed version of '(\<$\>)'.
(<$$>) :: (Functor f) => f a -> (a -> b) -> f b
(<$$>) = flip (<$>)

-- | Reversed version of '(<$)'.
($>) :: (Functor f) => f a -> b -> f b
($>) = flip (<$)
-}

-- | @bound (min,max)@ behaves like identity if the supplied value is between @min@ and @max@, otherwise it is replaced either by @min@ or by @max@.
bound :: (Ord a) => (a, a) -> a -> a
bound (min, max) x
  | x < min = min
  | x > max = max
  | otherwise = x

fromMaybeM_ :: (Monad m) => Maybe (m ()) -> m ()
fromMaybeM_ = fromMaybe (return ())

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> [a]
safeTail []     = []
safeTail (_:xs) = xs

maybeToEvent :: Maybe a -> Event a
maybeToEvent Nothing  = NoEvent
maybeToEvent (Just x) = Event x

eventToMaybe :: Event a -> Maybe a
eventToMaybe NoEvent   = Nothing
eventToMaybe (Event x) = Just x

eventToList :: Event [a] -> [a]
eventToList NoEvent   = []
eventToList (Event x) = x

curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a,b,c)

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c

curry4 :: ((a,b,c,d) -> e) -> a -> b -> c -> d -> e
curry4 f a b c d = f (a,b,c,d)

uncurry4 :: (a -> b -> c -> d -> e) -> (a,b,c,d) -> e
uncurry4 f (a,b,c,d) = f a b c d

curry5 :: ((a,b,c,d,e) -> f) -> a -> b -> c -> d -> e -> f
curry5 f a b c d e = f (a,b,c,d,e)

uncurry5 :: (a -> b -> c -> d -> e -> f) -> (a,b,c,d,e) -> f
uncurry5 f (a,b,c,d,e) = f a b c d e
