{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}

-- Contains all the information and functions necessary to run a Jack
-- port and exchange information through reactive values and Yampa.
module RMCA.Translator.Jack ( jackSetup
                            ) where

import           Control.Arrow
import qualified Control.Monad.Exception.Synchronous as Sync
import qualified Control.Monad.Trans.Class           as Trans
import           Data.CBRef
import           Data.Foldable
import qualified Data.IntMap                         as M
import           Data.Maybe
import           Data.ReactiveValue
import qualified Foreign.C.Error                     as E
import           Graphics.UI.Gtk
import           RMCA.IOClockworks
import           RMCA.Layer.LayerConf
import           RMCA.ReactiveValueAtomicUpdate
import           RMCA.Semantics
import           RMCA.Translator.Message
import           RMCA.Translator.RV
import           RMCA.Translator.Translator
import qualified Sound.JACK                          as Jack
import qualified Sound.JACK.Exception                as JackExc
import qualified Sound.JACK.MIDI                     as JMIDI

rmcaName :: String
rmcaName = "RMCA"

inPortName :: String
inPortName = "input"

outPortName :: String
outPortName = "output"

handleErrorJack :: JackExc.All -> IO ()
handleErrorJack _ = postGUIAsync $ do
  diag <- messageDialogNewWithMarkup
          Nothing [] MessageError ButtonsClose
          "No running instance of Jack could be found!"
  widgetShow diag
  resp <- dialogRun diag
  print resp
  mainQuit

-- Starts a default client with an input and an output port. Doesn't
-- do anything as such.
jackSetup :: (ReactiveValueAtomicUpdate board
              (M.IntMap ([Note],[Message])) IO
             , ReactiveValueRead tempo Tempo IO
             , ReactiveValueAtomicUpdate layerConfs (M.IntMap LayerConf) IO
             ) =>
             IOTick
          -> board
          -> tempo
          -> layerConfs
          -> IO ()
jackSetup tc boardQueue tempoRV layerMapRV = Sync.resolveT handleErrorJack $ do
  toProcessRV <- Trans.lift $ newCBRef []
  Jack.withClientDefault rmcaName $ \client ->
    Jack.withPort client outPortName $ \output ->
    Jack.withPort client inPortName  $ \input ->
    Jack.withProcess client (jackCallBack tc input output
                              toProcessRV boardQueue tempoRV layerMapRV) $
    Jack.withActivation client $ Trans.lift $ do
    putStrLn $ "Started " ++ rmcaName ++ " JACK client."
    Jack.waitForBreak
    return ()

-- The callback function. It pumps value out of the input port, mix
-- them with value coming from the machine itself and stuff them into
-- the output port. When this function is not running, events are
-- processed.
jackCallBack :: ( ReactiveValueAtomicUpdate toProcess [(Frames, RawMessage)] IO
                , ReactiveValueAtomicUpdate board
                  (M.IntMap ([Note],[Message])) IO
                , ReactiveValueRead tempo Tempo IO
                , ReactiveValueAtomicUpdate layerConfs (M.IntMap LayerConf) IO
                ) =>
                IOTick
             -> JMIDI.Port Jack.Input
             -> JMIDI.Port Jack.Output
             -> toProcess
             -> board
             -> tempo
             -> layerConfs
             -> Jack.NFrames
             -> Sync.ExceptionalT E.Errno IO ()
jackCallBack tc input output toProcessRV boardQueue tempoRV layerMapRV
  nframes@(Jack.NFrames nframesInt') = do
  let inMIDIRV = inMIDIEvent input nframes
      outMIDIRV = outMIDIEvent output nframes
      nframesInt = fromIntegral nframesInt' :: Int
  Trans.lift $ do
    tempo <- reactiveValueRead tempoRV
    inMIDI <- reactiveValueRead inMIDIRV
    let (unchangedMessages,toBeTreatedMessages) =
          break (\(_,m) -> fromMaybe False $ do
                    mess <- fromRawMessage m
                    return (isInstrument mess || isVolume mess)) inMIDI
    reactiveValueAppend toProcessRV unchangedMessages
    let (volume,instruments) = break (isInstrument . snd) $
          map (second (fromJust . fromRawMessage)) toBeTreatedMessages
    mapM_ ((\(Volume c v) -> reactiveValueUpdate layerMapRV
            (M.adjust (\(st,d,s) -> (st,d,s { volume = v }))
             (fromChannel c))) . snd) volume
    mapM_ ((\(Instrument c p) -> reactiveValueUpdate layerMapRV
            (M.adjust (\(st,d,s) -> (st,d,s { instrument = fromProgram p }))
              (fromChannel c))) . snd) instruments
    concat . toList . gatherMessages tempo nframesInt <$>
      reactiveValueEmpty boardQueue >>=
      reactiveValueAppend toProcessRV
    (go, old') <- schedule nframesInt <$> reactiveValueRead toProcessRV
    let old = map (first (+ (- nframesInt))) old'
    --putStrLn ("Out: " ++ show (map fst go))
    reactiveValueWrite outMIDIRV go
    reactiveValueWrite toProcessRV old
    tickIOTick tc
  --------------
