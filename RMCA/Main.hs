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
import RMCA.GUI.Buttons
import RMCA.Layer.Board
import RMCA.Layer.Layer
import RMCA.Semantics
import RMCA.Translator.Jack
import RMCA.Translator.Message
import RMCA.Translator.Translator
import Graphics.UI.Gtk.Layout.BackgroundContainer
import RMCA.GUI.Board

import Control.Monad
import Data.Ratio

floatConv :: (ReactiveValueReadWrite a b m,
              Real c, Real b, Fractional c, Fractional b) =>
             a -> ReactiveFieldReadWrite m c
floatConv = liftRW $ bijection (realToFrac, realToFrac)

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
  -- Main box
  mainBox <- hBoxNew True 0
  set window [ windowTitle := "Reactogon"
             --, windowDefaultWidth := 250
             --, windowDefaultHeight := 500
             , containerChild := mainBox
             , containerBorderWidth := 10
             ]

  settingsBox <- vBoxNew False 0
  boxPackEnd mainBox settingsBox PackNatural 0
  globalSettingsBox <- vBoxNew False 10
  boxPackStart settingsBox globalSettingsBox PackNatural 0
  tempoAdj <- adjustmentNew 96 0 200 1 1 1
  tempoLabel <- labelNew (Just "Tempo")
  boxPackStart globalSettingsBox tempoLabel PackNatural 0
  tempoScale <- hScaleNew tempoAdj
  boxPackStart globalSettingsBox tempoScale PackNatural 0
  scaleSetDigits tempoScale 0
  let tempoRV =
        bijection (floor, fromIntegral) `liftRW` scaleValueReactive tempoScale
  globalSep <- hSeparatorNew
  boxPackStart settingsBox globalSep PackNatural 0

  layerSettingsBox <- hBoxNew True 10
  boxPackStart settingsBox layerSettingsBox PackNatural 0

  layTempoBox <- hBoxNew False 10
  boxPackStart layerSettingsBox layTempoBox PackNatural 0
  layTempoLabel <- labelNew (Just "Layer tempo")
  labelSetAngle layTempoLabel 90
  boxPackStart layTempoBox layTempoLabel PackNatural 0
  layTempoScale <- vScaleNewWithRange 0 1 0.01
  boxPackStart layTempoBox layTempoScale PackNatural 0
  laySep <- hSeparatorNew

  strBox <- hBoxNew False 10
  boxPackStart layerSettingsBox strBox PackNatural 0
  strLabel <- labelNew (Just "Strength")
  labelSetAngle strLabel 90
  boxPackStart strBox strLabel PackNatural 0
  strAdj <- adjustmentNew 1 0 1 0.01 0.01 0
  layStrengthScale <- vScaleNew strAdj
  boxPackStart strBox layStrengthScale PackNatural 0

  bpbBox <- vBoxNew False 10
  boxPackStart layerSettingsBox bpbBox PackNatural 0
  bpbLabel <- labelNew (Just "Beat per bar")
  labelSetLineWrap bpbLabel True
  boxPackStart bpbBox bpbLabel PackNatural 0
  bpbAdj <- adjustmentNew 4 1 16 1 1 0
  bpbButton <- spinButtonNew bpbAdj 1 0
  boxPackStart bpbBox bpbButton PackNatural 0

  boxPackStart settingsBox laySep PackNatural 0

  layPitchRV <- newCBMVarRW 1
  let layTempoRV = floatConv $ scaleValueReactive layTempoScale
      strengthRV = floatConv $  scaleValueReactive layStrengthScale
      bpbRV = spinButtonValueIntReactive bpbButton
      f1 Layer { relTempo = d
               , relPitch = p
               , strength = s
               , beatsPerBar = bpb
               } = (d,p,s,bpb)
      f2 (d,p,s,bpb) = Layer { relTempo = d
                             , relPitch = p
                             , strength = s
                             , beatsPerBar = bpb
                             }
      layerRV =
        liftRW4 (bijection (f1,f2)) layTempoRV layPitchRV strengthRV bpbRV


  buttonBox <- hBoxNew True 10
  boxPackEnd settingsBox buttonBox PackNatural 0
  buttonPlay <- buttonNewFromStock gtkMediaPlay
  boxPackStart buttonBox buttonPlay PackRepel 0
  buttonPause <- buttonNewFromStock gtkMediaPause
  boxPackStart buttonBox buttonPause PackRepel 0
  buttonStop <- buttonNewFromStock gtkMediaStop
  boxPackStart buttonBox buttonStop PackRepel 0
  buttonRecord <- buttonNewFromStock gtkMediaRecord
  boxPackStart buttonBox buttonRecord PackRepel 0

  -- Board
  boardCont <- backgroundContainerNewWithPicture "ussr.png"
  containerAdd mainBox boardCont
  --boxPackStart mainBox boardCont PackNatural 0
  ------------------------------------------------------------------------------
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
