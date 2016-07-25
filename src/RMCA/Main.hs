{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables, TupleSections #-}

module Main where

import Control.Concurrent
import Data.ReactiveValue
import FRP.Yampa
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Board.BoardLink
import Graphics.UI.Gtk.Layout.BackgroundContainer
import Hails.Yampa
import RMCA.Auxiliary
import RMCA.Configuration
import RMCA.GUI.Board
import RMCA.GUI.Buttons
import RMCA.GUI.LayerSettings
import RMCA.GUI.MainSettings
import RMCA.GUI.NoteSettings
import RMCA.Layer.Board
import RMCA.Translator.Jack

main :: IO ()
main = do
  ------------------------------------------------------------------------------
  -- Main GUI
  ------------------------------------------------------------------------------
  initGUI
  window <- windowNew
  -- Main box
  mainBox <- hBoxNew False 10
  set window [ windowTitle := "Reactogon"
             , containerChild := mainBox
             , containerBorderWidth := 10
             ]
  windowMaximize window

  settingsBox <- vBoxNew False 0
  boxPackEnd mainBox settingsBox PackNatural 0
  (globalSettingsBox, tempoRV) <- globalSettings
  boxPackStart settingsBox globalSettingsBox PackNatural 0
  globalSep <- hSeparatorNew
  boxPackStart settingsBox globalSep PackNatural 0

  boardQueue <- newCBMVarRW mempty
  chanRV <- newCBMVarRW 0
  (layerSettingsVBox, layerRV, instrRV) <- layerSettings chanRV boardQueue
  boxPackStart settingsBox layerSettingsVBox PackNatural 0
  laySep <- hSeparatorNew
  boxPackStart settingsBox laySep PackNatural 0

  (buttonBox, playRV, stopRV, pauseRV, recordRV, confSaveRV, confLoadRV) <- getButtons
  boxPackEnd settingsBox buttonBox PackNatural 0

  -- Board
  boardCont <- backgroundContainerNew
  guiBoard <- attachGameRules =<< initGame
  centerBoard <- alignmentNew 0.5 0.5 0 0
  containerAdd centerBoard guiBoard
  containerAdd boardCont centerBoard
  boxPackStart mainBox boardCont PackNatural 0
  ------------------------------------------------------------------------------
  -- Board setup
  layer <- reactiveValueRead layerRV
  tempo <- reactiveValueRead tempoRV
  (boardRV, pieceArrRV, phRV) <- initBoardRV guiBoard

  handleSaveLoad tempoRV boardRV layerRV instrRV pieceArrRV confSaveRV confLoadRV

  boardRunRV <- newCBMVarRW BoardStop
  reactiveValueOnCanRead playRV $ reactiveValueWrite boardRunRV BoardStart
  reactiveValueOnCanRead stopRV $ reactiveValueWrite boardRunRV BoardStop
  board <- reactiveValueRead boardRV
  (inBoard, outBoard) <- yampaReactiveDual (board, layer, tempo, BoardStop) boardSF
  let tempoRV' = liftR2 (\bool t -> t * fromEnum (not bool)) pauseRV tempoRV
      inRV = liftR4 (,,,)
             boardRV layerRV tempoRV' boardRunRV
  --let inRV = onTick clock inRV
  inRV =:> inBoard
  reactiveValueOnCanRead outBoard $
    reactiveValueRead (liftR (event mempty (,[]) <<< snd <<< splitE) outBoard) >>=
    reactiveValueAppend boardQueue
  -- This needs to be set last otherwise phRV is written to, so
  -- inBoard is written to and the notes don't get played. There
  -- supposedly is no guaranty of order but apparently there is…
  fmap fst <^> outBoard >:> phRV
  putStrLn "Board started."
  -- Jack setup
  forkIO $ jackSetup tempoRV chanRV boardQueue
  widgetShowAll window
  pieceBox <- clickHandling pieceArrRV guiBoard =<< vBoxNew False 10
  ------------------------------------------------------------

  boxPackStart settingsBox pieceBox PackNatural 10
  onDestroy window mainQuit
  mainGUI
