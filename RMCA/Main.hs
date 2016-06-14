{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent
import Data.ReactiveValue
import FRP.Yampa
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Reactive
import Hails.Yampa
import RMCA.Auxiliary.Concurrent
import RMCA.Auxiliary.RV
import RMCA.Global.Clock
import RMCA.Layer.Board
import RMCA.Layer.Layer
import RMCA.Semantics
import RMCA.Translator.Jack
import RMCA.Translator.Message
import RMCA.Translator.Translator

import Control.Monad
import Data.Ratio

boardRVIO = newCBMVarRW $
    makeBoard [((0,0),  mkCell (ChDir True na1 NE)),
               ((1,1),  mkCellRpt (ChDir False na1 NW) 3),
               ((0,1),  mkCell (ChDir False na1 S))]
            {-makeBoard [((0,0), mkCell (ChDir True na1 N)),
               ((0,2), mkCellRpt (ChDir False na2 SE) 3),
               ((2,1), mkCell (ChDir False na1 SW)),
               ((1,1), mkCellRpt (ChDir False na1 N) 0) {- Skipped! -},
               ((0,4), mkCellRpt (ChDir True na1 N) (-1)) {- Rpt indef. -},
               ((0, -6), mkCell (ChDir True na1 N)),
               ((0, -2), mkCell (ChDir False na3 S) {- Silent -})]-}

na1 = NoteAttr {
          naArt = Accent13,
          naDur = 1 % 1,
          naOrn = Ornaments Nothing [] NoSlide
      }

na2 = NoteAttr {
          naArt = NoAccent,
          naDur = 1 % 1,
          naOrn = Ornaments Nothing [(10, MIDICVRnd)] SlideUp
      }

na3 = NoteAttr {
          naArt = Accent13,
          naDur = 0,
          naOrn = Ornaments Nothing [] NoSlide
      }


bpb :: Int
bpb = 4

newTempoRV :: IO (ReactiveFieldReadWrite IO Tempo)
newTempoRV = newCBMVarRW 200

main :: IO ()
main = do
  -- GUI
  initGUI
  window <- windowNew
  set window [ windowTitle := "Reactogon"
             ]
  globalSettingsBox <- vBoxNew False 10
  containerAdd window globalSettingsBox
  tempoAdj <- adjustmentNew 96 0 200 1 1 0
  tempoLabel <- labelNew (Just "Tempo")
  boxPackStart globalSettingsBox tempoLabel PackNatural 0
  tempoScale <- hScaleNew tempoAdj
  boxPackStart globalSettingsBox tempoScale PackGrow 0
  let tempoRV = bijection (floor, fromIntegral) `liftRW` scaleValueReactive tempoScale
  ------------------------------------------------------------------------------
  layerRV <- getDefaultLayerRV
  boardQueue <- newCBMVarRW []
  -- Board setup
  layer <- reactiveValueRead layerRV
  tempo <- reactiveValueRead tempoRV
  boardRV <- boardRVIO
  board <- reactiveValueRead boardRV
  (inBoard, outBoard) <- yampaReactiveDual (board, layer, tempo)
                         (boardSF $ startHeads board)
  let inRV = liftRW2 (bijection (\(x,y,z) -> (x,(y,z)), \(x,(y,z)) -> (x,y,z)))
             boardRV $ pairRW layerRV tempoRV
  clock <- mkClockRV 100
  clock ^:> inRV
  inRV =:> inBoard
  --reactiveValueOnCanRead outBoard (reactiveValueRead outBoard >>= print . ("Board out " ++) . show)
  reactiveValueOnCanRead outBoard $ do
    bq <- reactiveValueRead boardQueue
    ob <- reactiveValueRead $ liftR (event [] id) outBoard
    reactiveValueWrite boardQueue (bq ++ ob)
  -- /!\ To be removed.
  --reactiveValueOnCanRead outBoard (reactiveValueRead outBoard >>= print)
  putStrLn "Board started."
  -- Jack setup
  forkIO $ jackSetup tempoRV (constR 0) boardQueue
  widgetShowAll window
  onDestroy window mainQuit
  mainGUI
  --return ()
