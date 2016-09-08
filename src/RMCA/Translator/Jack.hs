{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}

-- Contains all the information and functions necessary to run a Jack
-- port and exchange information through reactive values and Yampa.
module RMCA.Translator.Jack ( jackSetup
                            ) where

import           Control.Arrow
import           Control.Concurrent.MVar
import qualified Control.Monad.Exception.Synchronous as Sync
import qualified Control.Monad.Trans.Class           as Trans
import           Data.Foldable
import qualified Data.IntMap                         as M
import           Data.ReactiveValue
import qualified Foreign.C.Error                     as E
import           Graphics.UI.Gtk
import           RMCA.Auxiliary
import           RMCA.Global.Clock
import           RMCA.IOClockworks
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
jackSetup :: (ReactiveValueReadWrite board
              (M.IntMap ([Note],[Message])) IO
             , ReactiveValueRead tempo Tempo IO) =>
             IOTick
          -> board
          -> tempo
          -> IO ()
jackSetup tc boardQueue tempoRV = Sync.resolveT handleErrorJack $ do
  toProcessRV <- Trans.lift $ newCBMVarRW []
  Jack.withClientDefault rmcaName $ \client ->
    Jack.withPort client outPortName $ \output ->
    Jack.withPort client inPortName  $ \input ->
    Jack.withProcess client (jackCallBack tc input output
                              toProcessRV boardQueue tempoRV) $
    Jack.withActivation client $ Trans.lift $ do
    putStrLn $ "Started " ++ rmcaName ++ " JACK client."
    --newEmptyMVar >>= takeMVar
    Jack.waitForBreak
    return ()

-- The callback function. It pumps value out of the input port, mix
-- them with value coming from the machine itself and stuff them into
-- the output port. When this function is not running, events are
-- processed.
jackCallBack :: ( ReactiveValueReadWrite toProcess [(Frames, RawMessage)] IO
                , ReactiveValueReadWrite board
                  (M.IntMap ([Note],[Message])) IO
                , ReactiveValueRead tempo Tempo IO) =>
                IOTick
             -> JMIDI.Port Jack.Input
             -> JMIDI.Port Jack.Output
             -> toProcess
             -> board
             -> tempo
             -> Jack.NFrames
             -> Sync.ExceptionalT E.Errno IO ()
jackCallBack tc input output toProcessRV boardQueue tempoRV
  nframes@(Jack.NFrames nframesInt') = do
  let inMIDIRV = inMIDIEvent input nframes
      outMIDIRV = outMIDIEvent output nframes
      nframesInt = fromIntegral nframesInt' :: Int
  Trans.lift $ do
    tempo <- reactiveValueRead tempoRV
    concat . toList . gatherMessages tempo nframesInt <$>
      reactiveValueRead boardQueue >>= \bq ->
      reactiveValueAppend toProcessRV bq >> putStrLn ("BoardQueue: " ++ show (map fst bq))
    reactiveValueEmpty  boardQueue
    (go, old') <- schedule nframesInt <$> reactiveValueRead toProcessRV
    let old = map (first (+ (- nframesInt))) old'
    putStrLn ("Out: " ++ show (map fst go))
    reactiveValueWrite outMIDIRV go
    reactiveValueWrite toProcessRV old
    tickIOTick tc
  --------------
