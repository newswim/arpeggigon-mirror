{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

module RMCA.Auxiliary.RV where

import Data.CBMVar
import Data.ReactiveValue
import FRP.Yampa
import Control.Monad
import RMCA.Auxiliary.Curry

leftSyncWith :: (ReactiveValueRead a b m, ReactiveValueWrite c d m) =>
                (b -> d) -> a -> c -> m ()
leftSyncWith f a c = reactiveValueOnCanRead a
  (reactiveValueRead a >>= reactiveValueWrite c . f)

(=:$:>) :: (ReactiveValueRead a b m, ReactiveValueWrite c d m) =>
           (b -> d) -> a -> c -> m ()
(=:$:>) = leftSyncWith

newCBMVarRW :: forall a. a -> IO (ReactiveFieldReadWrite IO a)
newCBMVarRW val = do
  mvar <- newCBMVar val
  let getter :: IO a
      getter = readCBMVar mvar
      setter :: a -> IO ()
      setter = writeCBMVar mvar
      notifier :: IO () -> IO ()
      notifier = installCallbackCBMVar mvar
  return $ ReactiveFieldReadWrite setter getter notifier

emptyRW :: (Monoid b, ReactiveValueReadWrite a b m) => a -> m b
emptyRW rv = do
  val <- reactiveValueRead rv
  reactiveValueWrite rv mempty
  return val

emptyW :: (Monoid b, ReactiveValueWrite a b m) => a -> m ()
emptyW rv = reactiveValueWrite rv mempty

onTick :: (ReactiveValueRead a b m, ReactiveValueRead c d m) =>
          a -> c -> ReactiveFieldRead m d
onTick notif rv = ReactiveFieldRead getter notifier
  where getter = reactiveValueRead rv
        notifier cb = do
          reactiveValueOnCanRead notif cb
          reactiveValueOnCanRead rv cb

addHandlerR :: (ReactiveValueRead a b m) =>
                  a
               -> (m () -> m())
               -> ReactiveFieldRead m b
addHandlerR x h = ReactiveFieldRead (reactiveValueRead x)
                  (\p -> reactiveValueOnCanRead x p >> h p)
{-
notif ^:> rv =
  reactiveValueOnCanRead notif (reactiveValueOnCanRead rv (return ()))
-}
-- Update when the value is an Event. It would be nice to have that
-- even for Maybe as well.
(>:>) :: (ReactiveValueRead a (Event b) IO, ReactiveValueWrite c b IO) =>
         a -> c -> IO ()
eventRV >:> rv = reactiveValueOnCanRead eventRV syncOnEvent
  where  syncOnEvent = do
           erv <- reactiveValueRead eventRV
           when (isEvent erv) $ reactiveValueWrite rv $ fromEvent erv
{-
liftR3 :: ( Monad m
          , ReactiveValueRead a b m
          , ReactiveValueRead c d m
          , ReactiveValueRead e f m) =>
          ((b,d,f) -> i)
       -> a
       -> c
       -> e
       -> ReactiveFieldRead m i
liftR3 f a b c = ReactiveFieldRead getter notifier
  where getter = do
          x1 <- reactiveValueRead a
          x2 <- reactiveValueRead b
          x3 <- reactiveValueRead c
          return $ f (x1, x2, x3)
        notifier p = reactiveValueOnCanRead a p >>
                     reactiveValueOnCanRead b p >>
                     reactiveValueOnCanRead c p
-}

liftW3 :: ( Monad m
          , ReactiveValueWrite a b m
          , ReactiveValueWrite c d m
          , ReactiveValueWrite e f m) =>
          (i -> (b,d,f))
       -> a
       -> c
       -> e
       -> ReactiveFieldWrite m i
liftW3 f a b c = ReactiveFieldWrite setter
  where setter x = do
          let (x1,x2,x3) = f x
          reactiveValueWrite a x1
          reactiveValueWrite b x2
          reactiveValueWrite c x3

liftRW3 :: ( ReactiveValueReadWrite a b m
           , ReactiveValueReadWrite c d m
           , ReactiveValueReadWrite e f m) =>
           BijectiveFunc i (b,d,f)
        -> a
        -> c
        -> e
        -> ReactiveFieldReadWrite m i
liftRW3 bij a b c =
  ReactiveFieldReadWrite setter getter notifier
  where ReactiveFieldRead getter notifier = liftR3 (curry3 f2) a b c
        ReactiveFieldWrite setter = liftW3 f1 a b c
        (f1, f2) = (direct bij, inverse bij)

liftR4 :: ( ReactiveValueRead a b m
          , ReactiveValueRead c d m
          , ReactiveValueRead e f m
          , ReactiveValueRead g h m) =>
          ((b,d,f,h) -> i)
       -> a
       -> c
       -> e
       -> g
       -> ReactiveFieldRead m i
liftR4 f a b c d = ReactiveFieldRead getter notifier
  where getter = do
          x1 <- reactiveValueRead a
          x2 <- reactiveValueRead b
          x3 <- reactiveValueRead c
          x4 <- reactiveValueRead d
          return $ f (x1, x2, x3, x4)
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
          (i -> (b,d,f,h))
       -> a
       -> c
       -> e
       -> g
       -> ReactiveFieldWrite m i
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
           BijectiveFunc i (b,d,f,h)
        -> a
        -> c
        -> e
        -> g
        -> ReactiveFieldReadWrite m i
liftRW4 bij a b c d =
  ReactiveFieldReadWrite setter getter notifier
  where ReactiveFieldRead getter notifier = liftR4 f2 a b c d
        ReactiveFieldWrite setter = liftW4 f1 a b c d
        (f1, f2) = (direct bij, inverse bij)
