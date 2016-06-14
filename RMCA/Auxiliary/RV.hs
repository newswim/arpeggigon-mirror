{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

module RMCA.Auxiliary.RV where

import Data.CBMVar
import Data.ReactiveValue
import FRP.Yampa
import Control.Monad

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

(^:>) :: (ReactiveValueRead a b m, ReactiveValueReadWrite c d m) =>
         a -> c -> m ()
notif ^:> rv = reactiveValueOnCanRead notif resync
  where resync = reactiveValueRead rv >>= reactiveValueWrite rv

-- Update when the value is an Event. It would be nice to have that
-- even for Maybe as well.
(>:>) :: (ReactiveValueRead a (Event b) m, ReactiveValueWrite c b m) =>
         a -> c -> m ()
eventRV >:> rv = reactiveValueOnCanRead eventRV syncOnEvent
  where  syncOnEvent = do
           erv <- reactiveValueRead eventRV
           when (isEvent erv) $ reactiveValueWrite rv $ fromEvent erv
