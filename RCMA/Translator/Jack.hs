-- Contains all the information and functions necessary to run a Jack
-- port and exchange information through reactive values and Yampa.
module RCMA.Translator.Jack ( jackSetup
                            ) where

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
import qualified Sound.JACK.Exception                as JExc
import qualified Sound.JACK.MIDI                     as JMIDI

import           Debug.Trace

rcmaName :: String
rcmaName = "RCMA"

inPortName :: String
inPortName = "input"

outPortName :: String
outPortName = "output"

-- Starts a default client with an input and an output port. Doesn't
-- do anything as such.
jackSetup :: ReactiveFieldRead IO (LTempo, Int, [Note])
          -> IO ()
jackSetup boardInRV = Jack.handleExceptions $ do
  toProcessRV <- Trans.lift $ toProcess <$> newCBMVar []
  Jack.withClientDefault rcmaName $ \client ->
    Jack.withPort client outPortName $ \output ->
    Jack.withPort client inPortName  $ \input ->
    Jack.withProcess client (jackCallBack client input output
                              toProcessRV boardInRV) $
    Jack.withActivation client $ Trans.lift $ do
    putStrLn $ "Started " ++ rcmaName ++ " JACK client."
    Jack.waitForBreak

{-
-- Loop that does nothing except setting up a callback function
-- (called when Jack is ready to take new inputs).
jackRun :: (JExc.ThrowsErrno e) =>
           Jack.Client
        -> (Jack.NFrames -> Sync.ExceptionalT E.Errno IO ())
        -> Sync.ExceptionalT e IO ()
jackRun client callback =
  Jack.withProcess client callback $ do
  Trans.lift $ putStrLn $ "Startedbbb " ++ rcmaName
  Trans.lift $ Jack.waitForBreak
-}
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
             -> ReactiveFieldRead IO (LTempo, Int, [Note])
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
  --Trans.lift (reactiveValueRead inMIDIRV >>= (print . map (fst)))
  -- We write the content of the input buffer to the input of a
  -- translation signal function.
  -- /!\ Should maybe be moved elsewhere
  (inRaw, outPure) <- Trans.lift $ yampaReactiveDual [] readMessages
  Trans.lift (inMIDIRV =:> inRaw)
  (tempo, chan, boardIn') <- Trans.lift $ reactiveValueRead boardInRV
  let boardIn = (zip (repeat 0) boardIn',[],[])
  outMIDI <- Trans.lift $ reactiveValueRead outPure
  -- We translate all signals to be sent into low level signals and
  -- write them to the output buffer.
  (inPure, outRaw) <- Trans.lift $ yampaReactiveDual
                      (defaultTempo, sr, chan, ([],[],[])) gatherMessages
  -- This should all go in its own IO action
  Trans.lift $ do
    reactiveValueWrite inPure (tempo, sr, chan, boardIn `mappend` outMIDI)
    reactiveValueRead outRaw <**>
      (mappend <$> reactiveValueRead toProcessRV) >>=
      reactiveValueWrite toProcessRV
    (go, old') <- schedule nframesInt <$> reactiveValueRead toProcessRV
    let old = map (BF.first (+ (- nframesInt))) old'
    reactiveValueWrite outMIDIRV go
    reactiveValueWrite toProcessRV old
  --------------
