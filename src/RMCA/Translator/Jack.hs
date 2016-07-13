{-# LANGUAGE FlexibleContexts #-}

-- Contains all the information and functions necessary to run a Jack
-- port and exchange information through reactive values and Yampa.
module RMCA.Translator.Jack ( jackSetup
                            ) where

import qualified Control.Monad.Exception.Synchronous as Sync
import qualified Control.Monad.Trans.Class           as Trans
import qualified Data.Bifunctor                      as BF
import           Data.CBMVar
import           Data.ReactiveValue
import qualified Foreign.C.Error                     as E
import           Hails.Yampa
import           RMCA.Auxiliary.RV
import           RMCA.Semantics
import           RMCA.Translator.Filter
import           RMCA.Translator.Message
import           RMCA.Translator.RV
import           RMCA.Translator.Translator
import qualified Sound.JACK                          as Jack
import qualified Sound.JACK.MIDI                     as JMIDI

rmcaName :: String
rmcaName = "RMCA"

inPortName :: String
inPortName = "input"

outPortName :: String
outPortName = "output"

-- Starts a default client with an input and an output port. Doesn't
-- do anything as such.
jackSetup :: ( ReactiveValueRead tempo LTempo IO
             , ReactiveValueRead channel Int IO
             , ReactiveValueReadWrite board ([Note],[Message]) IO) =>
             tempo
          -> channel
          -> board
          -> IO ()
jackSetup tempoRV chanRV boardInRV = Jack.handleExceptions $ do
  toProcessRV <- Trans.lift $ toProcess <$> newCBMVar []
  Jack.withClientDefault rmcaName $ \client ->
    Jack.withPort client outPortName $ \output ->
    Jack.withPort client inPortName  $ \input ->
    Jack.withProcess client (jackCallBack client input output
                              toProcessRV tempoRV chanRV boardInRV) $
    Jack.withActivation client $ Trans.lift $ do
    putStrLn $ "Started " ++ rmcaName ++ " JACK client."
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
  Trans.lift $ putStrLn $ "Startedbbb " ++ rmcaName
  Trans.lift $ Jack.waitForBreak
-}
defaultTempo :: Tempo
defaultTempo = 96

-- The callback function. It pumps value out of the input port, mix
-- them with value coming from the machine itself and stuff them into
-- the output port. When this function is not running, events are
-- processed.
jackCallBack :: ( ReactiveValueReadWrite toProcess [(Frames, RawMessage)] IO
                , ReactiveValueRead tempo LTempo IO
                , ReactiveValueRead channel Int IO
                , ReactiveValueReadWrite board ([Note],[Message]) IO) =>
                Jack.Client
             -> JMIDI.Port Jack.Input
             -> JMIDI.Port Jack.Output
             -> toProcess
             -> tempo
             -> channel
             -> board
             -> Jack.NFrames
             -> Sync.ExceptionalT E.Errno IO ()
jackCallBack client input output toProcessRV tempoRV chanRV outBoard
  nframes@(Jack.NFrames nframesInt') = do
  let inMIDIRV = inMIDIEvent input nframes
      outMIDIRV = outMIDIEvent output nframes
      nframesInt = fromIntegral nframesInt' :: Int
  -- This gets the sample rate of the client and the last frame number
  -- it processed. We then use it to calculate the current absolute time
  sr <- Trans.lift $ Jack.getSampleRate client
  --(Jack.NFrames lframeInt) <- Trans.lift $ Jack.lastFrameTime client
  --Trans.lift (reactiveValueRead inMIDIRV >>= (print . map (fst)))
  -- We write the content of the input buffer to the input of a
  -- translation signal function.
  -- /!\ Should maybe be moved elsewhere
  (inRaw, outPure) <- Trans.lift $ yampaReactiveDual [] readMessages
  Trans.lift (inMIDIRV =:> inRaw)
  tempo <- Trans.lift $ reactiveValueRead tempoRV
  chan <- Trans.lift $ reactiveValueRead chanRV
  (notes,ctrl) <- Trans.lift $ reactiveValueRead outBoard
  Trans.lift $ emptyRW outBoard
  let boardIn = (zip (repeat 0) notes, zip (repeat 0) ctrl, [])
  outMIDI <- Trans.lift $ reactiveValueRead outPure
  -- We translate all signals to be sent into low level signals and
  -- write them to the output buffer.
  (inPure, outRaw) <- Trans.lift $ yampaReactiveDual
                      (defaultTempo, sr, chan, ([],[],[])) gatherMessages
  -- This should all go in its own IO action
  Trans.lift $ do
    reactiveValueWrite inPure (tempo, sr, chan, boardIn `mappend` outMIDI)
    reactiveValueRead outRaw >>= reactiveValueAppend toProcessRV
    --map fst <$> reactiveValueRead toProcessRV >>= print . ("toProcess " ++) . show
    (go, old') <- schedule nframesInt <$> reactiveValueRead toProcessRV
    let old = map (BF.first (+ (- nframesInt))) old'
    reactiveValueWrite outMIDIRV go
    reactiveValueWrite toProcessRV old
  --------------
