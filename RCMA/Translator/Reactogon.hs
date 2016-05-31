module Main where

import Auxiliary
import MIDI
import ClientState
import Reactimation

import qualified Sound.JACK as Jack
import qualified Sound.JACK.MIDI as JMIDI
import qualified Sound.MIDI.Message as MIDI
import qualified Sound.MIDI.Message.Channel as Channel
import qualified Sound.MIDI.Message.Channel.Voice as Voice
import qualified Sound.MIDI.Message.Class.Construct as MidiCons

import Control.Concurrent
import Control.Monad
import qualified Control.Monad.Exception.Synchronous as Sync
import qualified Control.Monad.Trans.Class as Trans
import qualified Data.EventList.Absolute.TimeBody as EventListAbs
import qualified Data.EventList.Relative.TimeBody as EventList
import qualified Data.EventList.Relative.TimeMixed as EventListTM
import qualified Foreign.C.Error as E

import qualified Data.Map as M
import FRP.Yampa

import Debug.Trace
{-
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
-}

rcmaName :: String
rcmaName = "RCMA"

inPortName :: String
inPortName = "input"

outPortName :: String
outPortName = "output"

fsPortName :: String
fsPortName = "fluidsynth:midi"

main = do
  inState <- newMVar M.empty
  outState <- newMVar M.empty
  Jack.handleExceptions $
    Jack.withClientDefault rcmaName $ \client ->
    Jack.withPort client outPortName $ \output ->
    Jack.withPort client inPortName $ \input -> do
    clientState <- Trans.lift $ newEmptyMVar
    Jack.withProcess client
      (jackLoop client clientState inState outState input output) $
      Jack.withActivation client $ do
      frpid <- Trans.lift $ forkIO $ mainReact inState outState clientState
      Jack.connect client (rcmaName ++ ":" ++ outPortName) fsPortName
      Trans.lift $ putStrLn $ "Started " ++ rcmaName
      Trans.lift $ Jack.waitForBreak

jackLoop :: Jack.Client
         -> MVar ClientState -- ^ MVar containing the client state (rate and buff size)
         -> MVar EventQueue -- ^ MVar containing incoming events
         -> MVar EventQueue -- ^ MVar containing exiting events
         -> JMIDI.Port Jack.Input -- ^ Jack input port
         -> JMIDI.Port Jack.Output -- ^ Jack output port
         -> Jack.NFrames -- ^ Buffer size for the ports
         -> Sync.ExceptionalT E.Errno IO ()
jackLoop client clientState inRef outRef
         input output nframes@(Jack.NFrames nframesInt) = do
  Trans.lift $ print "Entering Jack."
  rate <- Trans.lift $ Jack.getSampleRate client
  lframe <- Trans.lift $ Jack.lastFrameTime client
  isEmptyState <- Trans.lift $ isEmptyMVar clientState
  let updateClient = if isEmptyState
                     then putMVar
                     else \c v -> void $ swapMVar c v
      rateD = fromIntegral rate
      (Jack.NFrames lframeInt) = lframe
      currentTime = fromIntegral lframeInt / rateD
  Trans.lift $ updateClient clientState $ ClientState { rate = rate
                                                      , buffSize = nframes
                                                      , clientClock = currentTime
                                                      }
  outEvents <- Trans.lift $ takeMVar outRef
  inEventsT <- JMIDI.readEventsFromPort input nframes
  let inEvents :: EventQueue
      inEvents = M.mapMaybe fromRawMessage $
        M.fromList $
        map (\(Jack.NFrames n,e) -> (currentTime + fromIntegral n/rateD, e)) $
        EventListAbs.toPairList inEventsT
  Trans.lift $ print "In the middle."
  Trans.lift $ putMVar inRef inEvents
  Trans.lift $ print "In the middle."
  let playableEvents = M.filterWithKey
        (\t _ -> t - currentTime > - fromIntegral nframesInt / rateD) $
        M.union inEvents outEvents
      (processableEvents, futureEvents) = breakMap currentTime playableEvents
      processableEvents' = M.toList processableEvents
  Trans.lift $ print currentTime
  Trans.lift $ putMVar outRef futureEvents
  let smartSub x y = if x < y then y - x else x - y
      (firstTime,_) = head processableEvents'
  Trans.lift $ print $
    map ((* rateD) . smartSub firstTime . fst) processableEvents'
  JMIDI.writeEventsToPort output nframes $
    EventListAbs.fromPairList $
    map (\(t,e) -> (Jack.NFrames $ floor $ rateD * smartSub t currentTime
                   , toRawMessage e)) $
    M.toList processableEvents
  Trans.lift $ print "Exiting Jack."
{-
    else JMIDI.writeEventsToPort output nframes $
         EventListAbs.mapTime Jack.NFrames $
         EventList.toAbsoluteEventList 0 $
         EventList.mapTime (\(Jack.NFrames n) -> n) $
         EventList.fromPairList processableEvents
-}
