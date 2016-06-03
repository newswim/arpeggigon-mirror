{-# LANGUAGE Arrows, PartialTypeSignatures #-}

-- Contains all the information and functions necessary to run a Jack
-- port and exchange information through reactive values and Yampa.
module RCMA.Translator.Jack where

import qualified Control.Monad.Exception.Synchronous as Sync
import qualified Control.Monad.Trans.Class           as Trans
import qualified Data.EventList.Absolute.TimeBody    as EventListAbs
import           Data.ReactiveValue
import qualified Foreign.C.Error                     as E
import           RCMA.Translator.Message
import           RCMA.Translator.RV
import qualified Sound.JACK                          as Jack
import qualified Sound.JACK.MIDI                     as JMIDI
import Hails.Yampa

rcmaName :: String
rcmaName = "RCMA"

inPortName :: String
inPortName = "input"

outPortName :: String
outPortName = "output"

-- Starts a default client with an input and an output port. Doesn't
-- do anything as such.
jackSetup :: IO ()
jackSetup = Jack.handleExceptions $ do
  toProcessQueue <- toProcess <$> newMVar []
  let toProcess = toProcess toProcessMVar
  Jack.withClientDefault rcmaName $ \client ->
  Jack.withPort client outPortName $ \output ->
  Jack.withPort client inPortName  $ \input ->
  jackRun client input output (jackCallBack client input output)

-- Loop that does nothing except setting up a callback function
-- (called when Jack is ready to take new inputs).
jackRun :: Jack.Client
        -> JMIDI.Port Jack.Input
        -> JMIDI.Port Jack.Output
        -> _
        -> _
jackRun client input output callback =
  Jack.withProcess client callback $ do
  Trans.lift $ putStrLn $ "Started " ++ rcmaName
  Trans.lift $ Jack.waitForBreak

-- The callback function. It pumps value out of the input port, mix
-- them with value coming from the machine itself and stuff them into
-- the output port. When this function is not running, events are
-- processed.
jackCallBack :: _
jackCallBack client input output toProcessQueue
  boardInRV
  nframes@(Jack.NFrames nframesInt) = do
  let inMIDIRV = inMIDIEvent input nframes
      outMIDIRV = outMIDIEvent output nframes
  -- This gets the sample rate of the client and the last frame number
  -- it processed. We then use it to calculate the current absolute time
  sr <- Trans.lift $ Jack.getSampleRate client
  (Jack.NFrames lframeInt) <- Trans.lift $ Jack.lastFrameTime client
  -- We write the content of the input buffer to the input of a
  -- translation signal function.
  (inRaw, outPure) <- yampaReactiveDual [] transFromRaw -- TODO Move in a
                             -- separate function
  inMIDIRV =:> inRaw
  board <- reactiveValueRead boardIn
  outMIDI <- reactiveValueRead outPure

  -- We translate all signals to be sent into low level signals and
  -- write them to the output buffer.
  (inPure, outRaw) <- yampaReactiveDual [] transToRaw
  reactiveValueWrite inPure (board ++ outMIDI)
  outRaw =:> outMIDIRV
  return ()
