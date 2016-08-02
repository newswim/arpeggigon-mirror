{-# LANGUAGE Arrows, FlexibleContexts, MultiParamTypeClasses #-}

module RMCA.Auxiliary where

import Control.Monad
import Data.CBMVar
import Data.Fixed
import Data.Maybe
import Data.ReactiveValue
import FRP.Yampa

--------------------------------------------------------------------------------
-- General functions
--------------------------------------------------------------------------------

($>) :: (Functor f) => f a -> b -> f b
($>) = flip (<$)

bound :: (Ord a) => (a, a) -> a -> a
bound (min, max) x
  | x < min = min
  | x > max = max
  | otherwise = x

fromMaybeM_ :: (Monad m) => Maybe (m ()) -> m ()
fromMaybeM_ = fromMaybe (return ())

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) = xs

maybeToEvent :: Maybe a -> Event a
maybeToEvent Nothing = NoEvent
maybeToEvent (Just x) = Event x

eventToMaybe :: Event a -> Maybe a
eventToMaybe NoEvent = Nothing
eventToMaybe (Event x) = Just x

--------------------------------------------------------------------------------
-- FRP
--------------------------------------------------------------------------------

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
  let makeEvent x x'
        | isNothing x' = NoEvent
        | otherwise = let x'' = fromJust x' in
            if x'' == x then NoEvent else Event x
  returnA -< makeEvent x x'

varFreqSine :: SF DTime Double
varFreqSine = sin ^<< (2*pi*) ^<< (`mod'` 1) ^<< integral <<^ (1/)

repeatedlyS :: a -> SF DTime (Event a)
repeatedlyS x = edgeBy (\a b -> if a * b < 0 then Just x else Nothing) 0
                <<< varFreqSine <<^ (2*)

-- Similar to onChange but contains its initial value in the first
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

updateRV :: (ReactiveValueReadWrite a b m) => a -> m ()
updateRV rv = reactiveValueRead rv >>= reactiveValueWrite rv

--------------------------------------------------------------------------------
-- Reactive Values
--------------------------------------------------------------------------------

newCBMVarRW :: a -> IO (ReactiveFieldReadWrite IO a)
newCBMVarRW val = do
  mvar <- newCBMVar val
  let getter = readCBMVar mvar
      setter = writeCBMVar mvar
      notifier = installCallbackCBMVar mvar
  return $ ReactiveFieldReadWrite setter getter notifier

reactiveValueAppend :: (Monoid b, ReactiveValueReadWrite a b m) =>
                       a -> b -> m ()
reactiveValueAppend rv v = do ov <- reactiveValueRead rv
                              reactiveValueWrite rv (ov `mappend` v)

reactiveValueWriteOnNotEq :: ( Eq b
                             , ReactiveValueReadWrite a b m) =>
                             a -> b -> m ()
reactiveValueWriteOnNotEq rv nv = do
  ov <- reactiveValueRead rv
  when (ov /= nv) $ reactiveValueWrite rv nv

emptyRW :: (Monoid b, ReactiveValueReadWrite a b m) => a -> m b
emptyRW rv = do
  val <- reactiveValueRead rv
  reactiveValueWrite rv mempty
  return val

-- Update when the value is an Event. It would be nice to have that
-- even for Maybe as well.
(>:>) :: (ReactiveValueRead a (Event b) IO, ReactiveValueWrite c b IO) =>
         a -> c -> IO ()
eventRV >:> rv = reactiveValueOnCanRead eventRV syncOnEvent
  where  syncOnEvent = do
           erv <- reactiveValueRead eventRV
           when (isEvent erv) $ reactiveValueWrite rv $ fromEvent erv

syncRightOnLeftWithBoth :: ( ReactiveValueRead a b m
                           , ReactiveValueReadWrite c d m
                           ) => (b -> d -> d) -> a -> c -> m ()
syncRightOnLeftWithBoth f l r = reactiveValueOnCanRead l $ do
  nl <- reactiveValueRead l
  or <- reactiveValueRead r
  reactiveValueWrite r (f nl or)

liftW3 :: ( Monad m
          , ReactiveValueWrite a b m
          , ReactiveValueWrite c d m
          , ReactiveValueWrite e f m) =>
          (i -> (b,d,f)) -> a -> c -> e -> ReactiveFieldWrite m i
liftW3 f a b c = ReactiveFieldWrite setter
  where setter x = do
          let (x1,x2,x3) = f x
          reactiveValueWrite a x1
          reactiveValueWrite b x2
          reactiveValueWrite c x3

liftRW3 :: ( ReactiveValueReadWrite a b m
           , ReactiveValueReadWrite c d m
           , ReactiveValueReadWrite e f m) =>
           BijectiveFunc i (b,d,f) -> a -> c -> e -> ReactiveFieldReadWrite m i
liftRW3 bij a b c =
  ReactiveFieldReadWrite setter getter notifier
  where ReactiveFieldRead getter notifier = liftR3 (curry3 f2) a b c
        ReactiveFieldWrite setter = liftW3 f1 a b c
        (f1, f2) = (direct bij, inverse bij)

liftR4 :: ( ReactiveValueRead a b m
          , ReactiveValueRead c d m
          , ReactiveValueRead e f m
          , ReactiveValueRead g h m) =>
          (b -> d -> f -> h -> i) -> a -> c -> e -> g -> ReactiveFieldRead m i
liftR4 f a b c d = ReactiveFieldRead getter notifier
  where getter = do
          x1 <- reactiveValueRead a
          x2 <- reactiveValueRead b
          x3 <- reactiveValueRead c
          x4 <- reactiveValueRead d
          return $ f x1 x2 x3 x4
        notifier p = do
          reactiveValueOnCanRead a p
          reactiveValueOnCanRead b p
          reactiveValueOnCanRead c p
          reactiveValueOnCanRead d p

liftW4 :: ( Monad m
          , ReactiveValueWrite a b m
          , ReactiveValueWrite c d m
          , ReactiveValueWrite e f m
          , ReactiveValueWrite g h m) =>
          (i -> (b,d,f,h)) -> a -> c -> e -> g -> ReactiveFieldWrite m i
liftW4 f a b c d = ReactiveFieldWrite setter
  where setter x = do
          let (x1,x2,x3,x4) = f x
          reactiveValueWrite a x1
          reactiveValueWrite b x2
          reactiveValueWrite c x3
          reactiveValueWrite d x4

liftRW4 :: ( ReactiveValueReadWrite a b m
           , ReactiveValueReadWrite c d m
           , ReactiveValueReadWrite e f m
           , ReactiveValueReadWrite g h m) =>
           BijectiveFunc i (b,d,f,h) -> a -> c -> e -> g
        -> ReactiveFieldReadWrite m i
liftRW4 bij a b c d =
  ReactiveFieldReadWrite setter getter notifier
  where ReactiveFieldRead getter notifier = liftR4 (curry4 f2) a b c d
        ReactiveFieldWrite setter = liftW4 f1 a b c d
        (f1, f2) = (direct bij, inverse bij)

liftR5 :: ( ReactiveValueRead a b m
          , ReactiveValueRead c d m
          , ReactiveValueRead e f m
          , ReactiveValueRead g h m
          , ReactiveValueRead i j m) =>
          (b -> d -> f -> h -> j -> k) -> a -> c -> e -> g -> i
       -> ReactiveFieldRead m k
liftR5 f a b c d e = ReactiveFieldRead getter notifier
  where getter = do
          x1 <- reactiveValueRead a
          x2 <- reactiveValueRead b
          x3 <- reactiveValueRead c
          x4 <- reactiveValueRead d
          x5 <- reactiveValueRead e
          return $ f x1 x2 x3 x4 x5
        notifier p = do
          reactiveValueOnCanRead a p
          reactiveValueOnCanRead b p
          reactiveValueOnCanRead c p
          reactiveValueOnCanRead d p
          reactiveValueOnCanRead e p

liftW5 :: ( Monad m
          , ReactiveValueWrite a b m
          , ReactiveValueWrite c d m
          , ReactiveValueWrite e f m
          , ReactiveValueWrite g h m
          , ReactiveValueWrite i j m) =>
          (k -> (b,d,f,h,j)) -> a -> c -> e -> g -> i -> ReactiveFieldWrite m k
liftW5 f a b c d e = ReactiveFieldWrite setter
  where setter x = do
          let (x1,x2,x3,x4,x5) = f x
          reactiveValueWrite a x1
          reactiveValueWrite b x2
          reactiveValueWrite c x3
          reactiveValueWrite d x4
          reactiveValueWrite e x5

liftRW5 :: ( ReactiveValueReadWrite a b m
           , ReactiveValueReadWrite c d m
           , ReactiveValueReadWrite e f m
           , ReactiveValueReadWrite g h m
           , ReactiveValueReadWrite i j m) =>
           BijectiveFunc k (b,d,f,h,j) -> a -> c -> e -> g -> i
        -> ReactiveFieldReadWrite m k
liftRW5 bij a b c d e =
  ReactiveFieldReadWrite setter getter notifier
  where ReactiveFieldRead getter notifier = liftR5 (curry5 f2) a b c d e
        ReactiveFieldWrite setter = liftW5 f1 a b c d e
        (f1, f2) = (direct bij, inverse bij)

--------------------------------------------------------------------------------
-- Curry and uncurry functions
--------------------------------------------------------------------------------

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
