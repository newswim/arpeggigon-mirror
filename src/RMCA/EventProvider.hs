{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TupleSections #-}

module RMCA.EventProvider ( EventProvider
                          , newEventProvider
                          , newEmptyEventProvider
                          , stopProviding
                          , getEPfromRV
                          , EventProviderQueue
                          , newEventProviderQueue
                          , newEmptyEventProviderQueue
                          , emptyProviderQueue
                          , getEPQfromRV
                          ) where

import Control.Concurrent.MVar
import Control.Monad
import Data.ReactiveValue
import FRP.Yampa
import RMCA.Auxiliary

newtype EventProvider a = EventProvider (MVar (Event a, [IO ()]))

newEventProvider :: Maybe a -> IO (EventProvider a)
newEventProvider = fmap EventProvider . newMVar . (,[]) . maybeToEvent

newEmptyEventProvider :: IO (EventProvider a)
newEmptyEventProvider = newEventProvider Nothing

-- Stop event production without triggering the callbacks.
stopProviding :: EventProvider a -> IO ()
stopProviding (EventProvider mvar) =
  modifyMVar_ mvar (\(_,cbs) -> return (NoEvent,cbs))

getEPfromRV :: (ReactiveValueRead a b IO) => a -> IO (EventProvider b)
getEPfromRV rv = do
  ep <- newEventProvider . Just =<< reactiveValueRead rv
  (Event <^> rv) =:> ep
  return ep

instance ReactiveValueRead (EventProvider a) (Event a) IO where
  reactiveValueRead (EventProvider mvar) =
    modifyMVar mvar $ \(mval,cbs) -> return ((NoEvent,cbs), mval)
  reactiveValueOnCanRead (EventProvider mvar) io =
    modifyMVar_ mvar $ \(mval,cbs) -> return (mval, cbs ++ [io])

instance ReactiveValueWrite (EventProvider a) (Event a) IO where
  reactiveValueWrite (EventProvider mvar) val = do
    modifyMVar_ mvar (\(_,cbs) -> return (val,cbs))
    readMVar mvar >>= sequence_ . snd

instance ReactiveValueReadWrite (EventProvider a) (Event a) IO where

newtype EventProviderQueue a = EventProviderQueue (MVar ([a], [IO ()]))

newEventProviderQueue :: [a] -> IO (EventProviderQueue a)
newEventProviderQueue = fmap EventProviderQueue . newMVar . (,[])

newEmptyEventProviderQueue :: IO (EventProviderQueue a)
newEmptyEventProviderQueue = newEventProviderQueue []

emptyProviderQueue :: EventProviderQueue a -> IO ()
emptyProviderQueue (EventProviderQueue mvar) =
  modifyMVar_ mvar (\(_,cbs) -> return ([],cbs))

getEPQfromRV :: (ReactiveValueRead a b IO) => a -> IO (EventProviderQueue b)
getEPQfromRV rv = do
  ep <- newEventProviderQueue . (:[]) =<< reactiveValueRead rv
  (Event <^> rv) =:> ep
  return ep

instance ReactiveValueRead (EventProviderQueue a) (Event a) IO where
  reactiveValueRead (EventProviderQueue mvar) =
    modifyMVar mvar popEventMVar
    where popEventMVar ([],cbs) = return (([],cbs), NoEvent)
          popEventMVar (el,cbs) = return ((init el,cbs), Event $ last el)
  reactiveValueOnCanRead (EventProviderQueue mvar) io =
    modifyMVar_ mvar $ \(mval,cbs) -> return (mval, cbs ++ [io])

instance ReactiveValueWrite (EventProviderQueue a) (Event a) IO where
  reactiveValueWrite (EventProviderQueue mvar) val = do
    when (isEvent val) $
      modifyMVar_ mvar $ \(mval,cbs) -> return (fromEvent val:mval,cbs)
    readMVar mvar >>= sequence_ . snd
