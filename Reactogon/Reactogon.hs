module Main where

import qualified MIDI as React

import qualified Sound.JACK as Jack
import qualified Sound.MIDI.Message as MIDI
import qualified Sound.JACK.MIDI as JMIDI
{-
import Data.IORef ( IORef
                  , newIORef
                  , readIORef
                  , writeIORef
                  )
-}
import Control.Concurrent
import qualified Foreign.C.Error as E
import qualified Data.EventList.Relative.TimeBody as EventList
import qualified Data.EventList.Absolute.TimeBody as EventListAbs
import qualified Data.EventList.Relative.TimeMixed as EventListTM
import qualified Control.Monad.Exception.Synchronous as Sync
import qualified Control.Monad.Trans.Class as Trans

import qualified Sound.MIDI.Message.Channel as Channel
import qualified Sound.MIDI.Message.Channel.Voice as Voice
import qualified Sound.MIDI.Message.Class.Construct as MidiCons

import FRP.Yampa

import Debug.Trace

-- | List of absolute times (at which events should occur) and events.
--   We assume that the list is sorted.
outLoop :: [(Time,MIDI.T)]
outLoop = concat [[(t,MIDI.Channel $ Channel.Cons
    { Channel.messageChannel = Channel.toChannel 4
    , Channel.messageBody =
        Channel.Voice $ Voice.NoteOn (Voice.toPitch 60) (Voice.toVelocity 100)
    }),(t+0.5,MIDI.Channel $ Channel.Cons
    { Channel.messageChannel = Channel.toChannel 4
    , Channel.messageBody =
        Channel.Voice $ Voice.NoteOff (Voice.toPitch 60) (Voice.toVelocity 100)
    })] | t <- [0,2..]]

reactogonName :: String
reactogonName = "Reactogon"

inPortName :: String
inPortName = "input"

outPortName :: String
outPortName = "output"

fsPortName :: String
fsPortName = "fluidsynth:midi"

main = do
  stateRef <- newMVar outLoop
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
    MVar [(Time,MIDI.T)] ->
    JMIDI.Port Jack.Output ->
    Jack.NFrames ->
    Sync.ExceptionalT E.Errno IO ()
process client stateRef output nframes@(Jack.NFrames nframesInt) =
  do
    rate <- Trans.lift $ Jack.getSampleRate client
    events <- Trans.lift $ takeMVar stateRef
    lframe <- Trans.lift $ Jack.lastFrameTime client
    let rateD = fromIntegral rate
        (Jack.NFrames lframeInt) = lframe
        currentTime = fromIntegral lframeInt / rateD
        playableEvents = filter
          (\(t,_) -> t - currentTime > - fromIntegral nframesInt / rateD) events
        (processableEvents, futureEvents) = break ((> currentTime) . fst) $
                                            playableEvents
    Trans.lift $ print currentTime
    Trans.lift $ putMVar stateRef futureEvents
    if null processableEvents
      then Trans.lift $ putStrLn "No events in queue."
      else Trans.lift $ putStrLn "Event!"
    let firstEventTime = fst $ head processableEvents
    Trans.lift $ print $ map ((* rateD) . smartSub currentTime . fst) processableEvents
    JMIDI.writeEventsToPort output nframes $
      EventListAbs.fromPairList $
      map (\(t,e) -> (Jack.NFrames $ floor $ rateD * smartSub t currentTime, e))
      processableEvents


smartSub x y = if x < y then y - x else x - y

{-
    else JMIDI.writeEventsToPort output nframes $
         EventListAbs.mapTime Jack.NFrames $
         EventList.toAbsoluteEventList 0 $
         EventList.mapTime (\(Jack.NFrames n) -> n) $
         EventList.fromPairList processableEvents
-}
