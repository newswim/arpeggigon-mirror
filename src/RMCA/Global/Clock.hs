module RMCA.Global.Clock where

import Control.Concurrent
import Control.Monad
import Data.CBMVar
import Data.ReactiveValue
import FRP.Yampa
import RMCA.Auxiliary
import RMCA.Semantics

{-
-- The initial value is arbitrary but never appears because the switch
-- is immediate.
metronome :: SF Tempo (Event Beat)
metronome = switch (repeatedly (tempoToQNoteIvl 120) ()
                    &&&
                    onChange') metronome'
  where metronome' :: Tempo -> SF Tempo (Event Beat)
        metronome' t = switch (repeatedly (tempoToQNoteIvl t) ()
                               &&&
                               onChange) metronome'
-}
metronome :: SF Tempo (Event Beat)
metronome = repeatedlyS () <<^ tempoToQNoteIvl

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
