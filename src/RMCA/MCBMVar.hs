{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TupleSections #-}

module RMCA.MCBMVar ( newMCBMVar
                    , readMCBMVar
                    , writeMCBMVar
                    , installCallbackMCBMVar
                    , removeCallbackMCBMVar
                    , MCBMVar
                    , HandlerId
                    ) where

import           Control.Concurrent.MVar
import           Control.Monad
import qualified Data.Map                as M
import           Data.ReactiveValue

type CallbackMap = M.Map Integer (IO ())

-- Carries a phantom type to avoid some errors where HandlerIds would
-- be applied to the wrong MCBMVar leading to strange results.
newtype HandlerId a = HandlerId Integer deriving(Eq, Show, Ord)

-- MVar executing actions when modified (highly inspired by CBMVar)
-- with the possibility of removing actions.
--
-- The callbacks to execute are stored in an integer indexed map, the
-- HandlerId stored with it is the index where the next callback will
-- be stored. This is to ensure that we never give the same HandlerId
-- several times, or we could have situations where a handler can
-- delete callback a and later callback b because callback b was added
-- behind at the same index where callback a was.
newtype MCBMVar a = MCBMVar (MVar (a, (HandlerId a,CallbackMap)))

newMCBMVar :: a -> IO (MCBMVar a)
newMCBMVar = (fmap MCBMVar) . newMVar . (,(HandlerId 0,M.empty))

readMCBMVar :: MCBMVar a -> IO a
readMCBMVar (MCBMVar x) = fmap fst (readMVar x)

runCallBacks :: MCBMVar a -> IO ()
runCallBacks (MCBMVar x) = readMVar x >>= sequence_ . M.elems . snd . snd

writeMCBMVar :: MCBMVar a -> a -> IO ()
writeMCBMVar w@(MCBMVar x) y = do
  takeMVar x >>= putMVar x . (y,) . snd
  runCallBacks w

installCallbackMCBMVar :: MCBMVar a -> IO () -> IO (HandlerId a)
installCallbackMCBMVar (MCBMVar x) io = do
  (val,(nhid'@(HandlerId nhid),cbs)) <- takeMVar x
  let ncbs = M.insertWith (\_ _ -> error "HandlerId already in use") nhid io cbs
  putMVar x (val,(HandlerId (nhid + 1), ncbs))
  return nhid'

removeCallbackMCBMVar :: MCBMVar a -> HandlerId a -> IO ()
removeCallbackMCBMVar (MCBMVar x) (HandlerId hid) = do
  (val,(nhid,cbs)) <- takeMVar x
  let ncbs = M.delete hid cbs
  putMVar x (val,(nhid,ncbs))

instance ReactiveValueRead (MCBMVar a) a IO where
  reactiveValueRead = readMCBMVar
  reactiveValueOnCanRead x io = void $ installCallbackMCBMVar x io

instance ReactiveValueWrite (MCBMVar a) a IO where
  reactiveValueWrite = writeMCBMVar

instance ReactiveValueReadWrite (MCBMVar a) a IO where
