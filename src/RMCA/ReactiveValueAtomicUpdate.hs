{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module RMCA.ReactiveValueAtomicUpdate where

import Control.Monad
import Data.CBRef
import Data.ReactiveValue

reactiveValueNonAtomicUpdate :: (ReactiveValueReadWrite a b m) =>
                                a -> (b -> b) -> m b
reactiveValueNonAtomicUpdate rv f = do
  val <- reactiveValueRead rv
  reactiveValueWrite rv $ f val
  return val

class (ReactiveValueReadWrite a b m) => ReactiveValueAtomicUpdate a b m where
  reactiveValueUpdate :: a -> (b -> b) -> m b

reactiveValueUpdate_ :: (ReactiveValueAtomicUpdate a b m) =>
                        a -> (b -> b) -> m ()
reactiveValueUpdate_ rv f = void $ reactiveValueUpdate rv f

reactiveValueAppend :: (Monoid b, ReactiveValueAtomicUpdate a b m) =>
                       a -> b -> m ()
reactiveValueAppend rv val = reactiveValueUpdate_ rv (`mappend` val)

reactiveValueEmpty :: (Monoid b, ReactiveValueAtomicUpdate a b m) =>
                      a -> m b
reactiveValueEmpty rv = reactiveValueUpdate rv (const mempty)

instance ReactiveValueRead (CBRef a) a IO where
  reactiveValueRead = readCBRef
  reactiveValueOnCanRead = installCallbackCBRef

instance ReactiveValueWrite (CBRef a) a IO where
  reactiveValueWrite = writeCBRef

instance ReactiveValueReadWrite (CBRef a) a IO where

instance ReactiveValueAtomicUpdate (CBRef a) a IO where
  reactiveValueUpdate rv f = atomicModifyCBRef rv (\x -> (f x, x))
