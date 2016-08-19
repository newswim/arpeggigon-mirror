{-# LANGUAGE MultiParamTypeClasses #-}

module RMCA.Global.Clock ( AbsBeat
                         , maxAbsBeat
                         , metronome
                         , tempoToQNoteIvl
                         , TickableClock
                         , newTickableClock
                         , tickClock
                         ) where

import Control.Concurrent
import Control.Monad
import Data.CBMVar
import Data.ReactiveValue
import FRP.Yampa
import RMCA.Auxiliary
import RMCA.Semantics

-- The absolute beat is the beat number of the global clock, there are
-- 16 starting from 1.
type AbsBeat = BeatNo

maxAbsBeat :: AbsBeat
maxAbsBeat = 16

-- The global system tempo beats every 16th note, each beat is tagged
-- with a beat number modulo sixteen. Each layer is then beating at
-- its own fraction, discarding the unecessary beats.
metronome :: SF Tempo (Event AbsBeat)
metronome = accumBy (\pb _ -> nextBeatNo maxAbsBeat pb) 1 <<<
            repeatedlyS () <<^ (15*) <<^ (1/) <<^ fromIntegral

-- Tempo is the number of quarter notes per minute.
tempoToQNoteIvl :: Tempo -> DTime
tempoToQNoteIvl = (15/) . fromIntegral

type TickingClock = (CBMVar (), ThreadId)

-- Make a clock that will execute any IO when it updates.
mkClockGeneric :: IO () -> DTime -> IO TickingClock
mkClockGeneric io d = do
  n <- newCBMVar ()
  tid <- forkIO $ forever $  do
    threadDelay dInt
    modifyCBMVar n return
    io
  return (n, tid)
  where dInt = floor $ d * 1000

-- Ticking clock in the IO monad, sending callbacks every t milliseconds.
mkClock :: DTime -> IO TickingClock
mkClock = mkClockGeneric (return ())

-- For debugging purposes.
mkClockDebug :: DTime -> IO TickingClock
mkClockDebug = mkClockGeneric (putStrLn "Ping !")

clockRV :: TickingClock -> ReactiveFieldRead IO ThreadId
clockRV (mvar, tid) = ReactiveFieldRead (return tid)
                      (installCallbackCBMVar mvar)

mkClockRV :: DTime -> IO (ReactiveFieldRead IO ThreadId)
mkClockRV d = clockRV <$> mkClock d

stopClock :: TickingClock -> IO ()
stopClock (_,t) = killThread t

-- | A clock that can be written to.
newtype TickableClock = TickableClock (CBMVar ())

tickClock :: TickableClock -> IO ()
tickClock (TickableClock cl) = writeCBMVar cl ()

newTickableClock :: IO TickableClock
newTickableClock = TickableClock <$> newCBMVar ()

instance ReactiveValueRead TickableClock () IO where
  reactiveValueRead _ = return ()
  reactiveValueOnCanRead (TickableClock tc) = installCallbackCBMVar tc
