{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}

module RMCA.Auxiliary.ReactiveValue where

import Control.Monad
import Data.CBMVar
import Data.CBRef
import Data.ReactiveValue
import FRP.Yampa
import RMCA.Auxiliary.Misc

-- |
-- = Auxiliary functions for manipulating reactive values

-- | Creates a new 'CBMVar' wrapped into a reactive field.
newCBMVarRW :: a -> IO (ReactiveFieldReadWrite IO a)
newCBMVarRW val = do
  mvar <- newCBMVar val
  let getter = readCBMVar mvar
      setter = writeCBMVar mvar
      notifier = installCallbackCBMVar mvar
  return $ ReactiveFieldReadWrite setter getter notifier

-- | Writes a value to a reactive value if the value is different from the one already in the reactive value.
reactiveValueWriteOnNotEq :: ( Eq b
                             , ReactiveValueReadWrite a b m) =>
                             a -> b -> m ()
reactiveValueWriteOnNotEq rv nv = do
  ov <- reactiveValueRead rv
  when (ov /= nv) $ reactiveValueWrite rv nv

-- | Relation that will update when the value is an 'Event'.
(>:>) :: (ReactiveValueRead a (Event b) IO, ReactiveValueWrite c b IO) =>
         a -> c -> IO ()
eventRV >:> rv = reactiveValueOnCanRead eventRV syncOnEvent
  where  syncOnEvent = do
           erv <- reactiveValueRead eventRV
           when (isEvent erv) $ reactiveValueWrite rv $ fromEvent erv

-- | When the reactive value on the left changes, the value on the right is updated using the value it contains and the value on the left with the provided function.
syncRightOnLeftWithBoth :: ( ReactiveValueRead a b m
                           , ReactiveValueReadWrite c d m
                           ) => (b -> d -> d) -> a -> c -> m ()
syncRightOnLeftWithBoth f l r = reactiveValueOnCanRead l $ do
  nl <- reactiveValueRead l
  or <- reactiveValueRead r
  reactiveValueWrite r (f nl or)

-- | Forces to update an reactive value by writing to it with the value it contains.
updateRV :: (ReactiveValueReadWrite a b m) => a -> m ()
updateRV rv = reactiveValueRead rv >>= reactiveValueWrite rv

floatConv :: (ReactiveValueReadWrite a b m,
              Real c, Real b, Fractional c, Fractional b) =>
             a -> ReactiveFieldReadWrite m c
floatConv = liftRW $ bijection (realToFrac, realToFrac)

swapHandlerStorage :: (ReactiveValueReadWrite a b IO) =>
                      a -> IO (ReactiveFieldReadWrite IO b)
swapHandlerStorage rv = do
  ioref <- newCBRef ()
  let setter val = reactiveValueWrite rv val >> writeCBRef ioref ()
      getter     = reactiveValueRead rv
      notifier   = installCallbackCBRef ioref
  return $ ReactiveFieldReadWrite setter getter notifier

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
