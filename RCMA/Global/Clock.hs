module RCMA.Global.Clock where

import Control.Concurrent
import Control.Monad
import Data.CBMVar
import Data.ReactiveValue
import FRP.Yampa
import RCMA.Auxiliary.Auxiliary
import RCMA.Semantics

tempo :: Tempo -> SF () Tempo
tempo = constant

-- The initial value is arbitrary but never appears because the switch
-- is immediate.
metronome :: SF Tempo (Event Beat)
metronome = switch (repeatedly (tempoToDTime 60) ()
                    &&&
                    onChange') metronome'
  where metronome' :: Tempo -> SF Tempo (Event Beat)
        metronome' t = switch (repeatedly (4 * tempoToDTime t) ()
                               &&&
                               onChange) metronome'

-- Tempo is the number of whole notes per minute.
tempoToDTime :: Tempo -> DTime
tempoToDTime = (15/) . fromIntegral

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
  where dInt = floor $ d * (10^3)

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
