module RCMA.Auxiliary.Concurrent where

import Control.Concurrent
import Control.Concurrent.MVar

forkChild :: IO () -> IO (MVar ())
forkChild io = do
  mvar <- newEmptyMVar
  forkFinally io (\_ -> putMVar mvar ())
  return mvar
