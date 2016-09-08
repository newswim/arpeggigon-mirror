{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TupleSections #-}

module RMCA.EventProvider ( EventProvider
                          , newEventProvider
                          , stopProviding
                          , getEPfromRV
                          ) where

import Control.Concurrent.MVar
import Data.ReactiveValue
import FRP.Yampa
import RMCA.Auxiliary

newtype EventProvider a = EventProvider (MVar (Event a, [IO ()]))

newEventProvider :: Maybe a -> IO (EventProvider a)
newEventProvider = fmap EventProvider . newMVar . (,[]) . maybeToEvent

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
