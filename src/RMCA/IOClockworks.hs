{-# LANGUAGE MultiParamTypeClasses #-}

module RMCA.IOClockworks ( IOMetronome
                         , mkClockGeneric
                         , mkClock
                         , mkClockDebug
                         , stopIOMetronome
                         , IOTick
                         , newIOTick
                         , tickIOTick
                         ) where

import Control.Concurrent
import Control.Monad
import Data.ReactiveValue
import FRP.Yampa          (DTime)

-- A reactive value carrying unit that ticks at a regular pace. On a
-- tick, it executes IO actions attached to it with
-- reactiveValueOnCanRead.
newtype IOMetronome = IOMetronome (MVar [IO ()], ThreadId)

instance ReactiveValueRead IOMetronome () IO where
  reactiveValueRead _ = return ()
  reactiveValueOnCanRead (IOMetronome (mvar,_)) io =
    modifyMVar_ mvar (\cbs -> return (cbs ++ [io]))

-- Make a clock that will execute any IO when it updates.
mkClockGeneric :: IO () -> DTime -> IO IOMetronome
mkClockGeneric io d = do
  n <- newMVar []
  tid <- forkIO $ forever $  do
    threadDelay dInt
    readMVar n >>= sequence_
    io
  return $ IOMetronome (n, tid)
  where dInt = floor $ d * 1000

-- Ticking clock in the IO monad, sending callbacks every t milliseconds.
mkClock :: DTime -> IO IOMetronome
mkClock = mkClockGeneric (return ())

-- For debugging purposes.
mkClockDebug :: DTime -> IO IOMetronome
mkClockDebug = mkClockGeneric (putStrLn "Ping!")

stopIOMetronome :: IOMetronome -> IO ()
stopIOMetronome (IOMetronome (_,tid)) = killThread tid

newtype IOTick = IOTick (MVar [IO ()])

newIOTick :: IO IOTick
newIOTick = fmap IOTick (newMVar [])

tickIOTick :: IOTick -> IO ()
tickIOTick (IOTick mvar) = readMVar mvar >>= sequence_

instance ReactiveValueRead IOTick () IO where
  reactiveValueRead _ = return ()
  reactiveValueOnCanRead (IOTick mvar) io =
    modifyMVar_ mvar (\cbs -> return (cbs ++ [io]))
