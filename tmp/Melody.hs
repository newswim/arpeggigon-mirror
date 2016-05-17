module Main where

import qualified Sound.JACK.MIDI as JackMidi
import qualified Sound.JACK as Jack
import qualified Sound.MIDI.Message as MIDI
import qualified Sound.MIDI.Message.Channel as Channel
import qualified Sound.MIDI.Message.Channel.Voice as Voice
import qualified Sound.MIDI.Message.Class.Construct as MidiCons

import qualified Control.Monad.Exception.Synchronous as Sync
import qualified Control.Monad.Trans.Class as Trans

import qualified Data.EventList.Absolute.TimeBody as AbsEventList
import qualified Data.EventList.Relative.TimeBody as EventList
import qualified Data.EventList.Relative.TimeMixed as EventListTM
import qualified Numeric.NonNegative.Wrapper as NonNegW
import Data.IORef ( IORef
                  , newIORef
                  , readIORef
                  , writeIORef
                  )

import qualified Foreign.C.Error as E

import System.Environment (getProgName)


scale :: [Channel.Pitch]
scale = map Channel.toPitch [60, 62, 64, 65, 67, 69, 71, 72]

-- events :: EventList.T Jack.NFrames MIDI.T
eventLoop :: EventList.T NonNegW.Double MIDI.T
eventLoop =
    EventList.fromPairList $
    concatMap
        (\p ->
            let note on =
                    MidiCons.note (Channel.toChannel 0)
                        (Voice.normalVelocity, p, on)
            in  [(0, note True), (0.1, note False)])
        scale

{-
eventList :: Jack.NFrames -> EventList.T Jack.NFrames MIDI.T
eventList nframes = EventList.fromPairList $
  zip (map ((mappend nframes) . Jack.NFrames) [22050,44100..] ) $
  [MIDI.Channel $ Channel.Cons
    { Channel.messageChannel = Channel.toChannel 0
    , Channel.messageBody =
        Channel.Voice $ Voice.NoteOn (Voice.toPitch s) (Voice.toVelocity 100)
    } | s <- scale]
-}

main :: IO ()
main = do
    name <- getProgName
    stateRef <- newIORef (EventList.cycle eventLoop)
    Jack.handleExceptions $
        Jack.withClientDefault name $ \client ->
        Jack.withPort client "output" $ \output -> do
        Jack.withProcess client (process client stateRef output) $
            Jack.withActivation client $ do
                --Jack.connect client "basic:output" "Midimon:input"
                Trans.lift $ putStrLn $ "started " ++ name ++ "..."
                Trans.lift $ Jack.waitForBreak

process ::
    Jack.Client ->
    IORef (EventList.T NonNegW.Double MIDI.T) ->
    JackMidi.Port Jack.Output ->
    Jack.NFrames ->
    Sync.ExceptionalT E.Errno IO ()
process client stateRef output nframes@(Jack.NFrames nframesInt) = do
    rate <- fmap fromIntegral $ Trans.lift $ Jack.getSampleRate client
    events <- Trans.lift $ readIORef stateRef
    let (currentEvents, futureEvents) =
            EventListTM.splitAtTime (fromIntegral nframesInt / rate) events
    Trans.lift $ writeIORef stateRef futureEvents
    JackMidi.writeEventsToPort output nframes $
        AbsEventList.mapTime (Jack.NFrames . NonNegW.toNumber) $
        EventList.toAbsoluteEventList 0 $
        EventList.resample rate $
        fst $ EventListTM.viewTimeR currentEvents
