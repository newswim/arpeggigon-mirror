{-# LANGUAGE Arrows, PartialTypeSignatures #-}

-- Contains all the information and functions necessary to run a Jack
-- port and exchange information through reactive values and Yampa.
module RCMA.Translator.Jack where

import           Control.Applicative                 ((<**>))
import qualified Control.Monad.Exception.Synchronous as Sync
import qualified Control.Monad.Trans.Class           as Trans
import qualified Data.Bifunctor                      as BF
import           Data.CBMVar
import qualified Data.EventList.Absolute.TimeBody    as EventListAbs
import           Data.ReactiveValue
import qualified Foreign.C.Error                     as E
import           Hails.Yampa
import           RCMA.Semantics
import           RCMA.Translator.Filter
import           RCMA.Translator.Message
import           RCMA.Translator.RV
import           RCMA.Translator.Translator
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
--jackSetup :: _
jackSetup boardInRV = Jack.handleExceptions $ do
  toProcessRV <- Trans.lift $ toProcess <$> newCBMVar []
  Jack.withClientDefault rcmaName $ \client ->
    Jack.withPort client outPortName $ \output ->
    Jack.withPort client inPortName  $ \input ->
    jackRun client input output
    (jackCallBack client input output toProcessRV boardInRV)

-- Loop that does nothing except setting up a callback function
-- (called when Jack is ready to take new inputs).
{-jackRun :: Jack.Client
        -> JMIDI.Port Jack.Input
        -> JMIDI.Port Jack.Output
        -> _
        -> _-}
jackRun client input output callback =
  Jack.withProcess client callback $ do
  Trans.lift $ putStrLn $ "Started " ++ rcmaName
  Trans.lift $ Jack.waitForBreak

defaultTempo :: Tempo
defaultTempo = 96

-- The callback function. It pumps value out of the input port, mix
-- them with value coming from the machine itself and stuff them into
-- the output port. When this function is not running, events are
-- processed.
jackCallBack :: Jack.Client
             -> JMIDI.Port Jack.Input
             -> JMIDI.Port Jack.Output
             -> ReactiveFieldReadWrite IO [(Frames, RawMessage)]
             -> ReactiveFieldRead IO (LTempo, Int, [(Frames, RawMessage)])
             -> Jack.NFrames
             -> Sync.ExceptionalT E.Errno IO ()
jackCallBack client input output toProcessRV boardInRV
  nframes@(Jack.NFrames nframesInt') = do
  let inMIDIRV = inMIDIEvent input nframes
      outMIDIRV = outMIDIEvent output nframes
      nframesInt = fromIntegral nframesInt' :: Int
  -- This gets the sample rate of the client and the last frame number
  -- it processed. We then use it to calculate the current absolute time
  sr <- Trans.lift $ Jack.getSampleRate client
  (Jack.NFrames lframeInt) <- Trans.lift $ Jack.lastFrameTime client
  -- We write the content of the input buffer to the input of a
  -- translation signal function.
  -- /!\ Should be moved elsewhere
  (inRaw, outPure) <- Trans.lift $ yampaReactiveDual [] readMessages
  Trans.lift (inMIDIRV =:> inRaw)
  (tempo, chan, boardIn') <- Trans.lift $ reactiveValueRead boardInRV
  let boardIn = ([],[],boardIn')
  outMIDI <- Trans.lift $ reactiveValueRead outPure
  -- We translate all signals to be sent into low level signals and
  -- write them to the output buffer.
  (inPure, outRaw) <- Trans.lift $ yampaReactiveDual
                      (defaultTempo, sr, chan, ([],[],[])) gatherMessages
  Trans.lift $ reactiveValueWrite inPure
               (tempo, sr, chan, (boardIn `mappend` outMIDI))
  Trans.lift (reactiveValueRead outRaw <**>
              (mappend <$> reactiveValueRead toProcessRV) >>=
              reactiveValueWrite toProcessRV)
  Trans.lift $ do
    (go, old') <- schedule nframesInt <$> reactiveValueRead toProcessRV
    let old = map (BF.first (\x -> x - nframesInt)) old'
    reactiveValueWrite outMIDIRV go
    reactiveValueWrite toProcessRV old
  return ()
