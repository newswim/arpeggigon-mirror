module Main where

import qualified MIDI as React

import qualified Sound.JACK as Jack
import qualified Sound.MIDI.Message as MIDI
import qualified Sound.JACK.MIDI as JMIDI
import Data.IORef ( IORef
                  , newIORef
                  , readIORef
                  , writeIORef
                  )
import qualified Foreign.C.Error as E
import qualified Data.EventList.Relative.TimeBody as EventList
import qualified Data.EventList.Absolute.TimeBody as EventListAbs
import qualified Data.EventList.Relative.TimeMixed as EventListTM
import qualified Control.Monad.Exception.Synchronous as Sync
import qualified Control.Monad.Trans.Class as Trans

eventLoop :: [(Jack.NFrames,MIDI.T)]
eventLoop = []

reactogonName :: String
reactogonName = "Reactogon"

inPortName :: String
inPortName = "input"

outPortName :: String
outPortName = "output"

fsPortName :: String
fsPortName = "fluidsynth:midi"

main = do
  stateRef <- newIORef eventLoop
  Jack.handleExceptions $
    Jack.withClientDefault reactogonName $ \client ->
    Jack.withPort client outPortName $ \output -> do
    Jack.withProcess client (process client stateRef output) $
      Jack.withActivation client $ do
        Jack.connect client (reactogonName ++ ":" ++ outPortName) fsPortName
        Trans.lift $ putStrLn $ "Started " ++ reactogonName
        Trans.lift $ Jack.waitForBreak

process ::
    Jack.Client ->
    IORef [(Jack.NFrames,MIDI.T)] ->
    JMIDI.Port Jack.Output ->
    Jack.NFrames ->
    Sync.ExceptionalT E.Errno IO ()
process client stateRef output nframes@(Jack.NFrames nframesInt) = do
  rate <- Trans.lift $ Jack.getSampleRate client
  events <- Trans.lift $ readIORef stateRef
  let (processableEvents, futureEvents) =
        break (\(Jack.NFrames n,_) ->
                 n < floor (fromIntegral nframesInt / fromIntegral rate)) events
  Trans.lift $ writeIORef stateRef futureEvents
  if null processableEvents
    then Trans.lift $ putStrLn "No events in queue."
    else JMIDI.writeEventsToPort output nframes $
         EventListAbs.mapTime Jack.NFrames $
         EventList.toAbsoluteEventList 0 $
         EventList.mapTime (\(Jack.NFrames n) -> n) $
         EventList.fromPairList processableEvents
