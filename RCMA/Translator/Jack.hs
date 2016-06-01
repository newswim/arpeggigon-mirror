{-# LANGUAGE Arrows #-}

-- Contains all the information and functions necessary to run a Jack
-- port and exchange information through reactive values and Yampa.
module RCMA.Translator.Jack where

import qualified Control.Monad.Exception.Synchronous as Sync
import qualified Control.Monad.Trans.Class           as Trans
import qualified Data.EventList.Absolute.TimeBody    as EventListAbs
import           Data.ReactiveValue
import qualified Foreign.C.Error                     as E
import           RCMA.Translator.Message
import qualified Sound.JACK                          as Jack
import qualified Sound.JACK.MIDI                     as JMIDI

rcmaName :: String
rcmaName = "RCMA"

inPortName :: String
inPortName = "input"

outPortName :: String
outPortName = "output"

-- Starts a default client with an input and an output port. Doesn't
-- do anything as such.
jackSetup :: IO ()
jackSetup = Jack.handleExceptions $
  Jack.withClientDefault rcmaName $ \client ->
  Jack.withPort client outPortName $ \output ->
  Jack.withPort client inPortName  $ \input ->
  jackRun client input output (jackCallBack client input output)

-- Loop that does nothing except setting up a callback function
-- (called when Jack is ready to take new inputs).
jackRun client input output callback =
  Jack.withProcess client callback $ do
  Trans.lift $ putStrLn $ "Started " ++ rcmaName
  Trans.lift $ Jack.waitForBreak

-- The callback function. It pumps value out of the input port, mix
-- them with value coming from the machine itself and stuff them into
-- the output port. When this function is not running, events are
-- processed.
jackCallBack client input output nframes@(Jack.NFrames nframesInt) = do
  -- This gets the sample rate of the client and the last frame number
  -- it processed. We then use it to calculate the current absolute time
  sr <- Trans.lift $ Jack.getSampleRate client
  (Jack.NFrames lframeInt) <- Trans.lift $ Jack.lastFrameTime client
  -- Read the events in the input buffer. /!\ This is in a weird
  -- structure that we dismount right after into a list.
  inBuffT <- JMIDI.readEventsFromPort input nframes
  let rateD = fromIntegral sr
      inBuff :: [(Frames, RawMessage)]
      inBuff = map (\(Jack.NFrames n,e) -> (fromIntegral n, e)) $
               EventListAbs.toPairList inBuffT

  return ()
