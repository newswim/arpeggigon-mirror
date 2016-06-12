{-# LANGUAGE ScopedTypeVariables #-}

module RMCA.Auxiliary.RV where

import Data.CBMVar
import Data.ReactiveValue

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
