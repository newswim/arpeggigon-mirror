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
import RMCA.GUI.MultiBoard
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

  (   buttonBox
    , playRV, stopRV, pauseRV, recordRV
    , confSaveRV, confLoadRV
    , addLayerRV, rmLayerRV ) <- getButtons
  boxPackEnd settingsBox buttonBox PackNatural 0

  boardQueue <- newCBMVarRW mempty
  (layerSettingsVBox, layerMCBMVar, instrMCBMVar) <- layerSettings boardQueue
  boxPackStart settingsBox layerSettingsVBox PackNatural 0
  laySep <- hSeparatorNew
  boxPackStart settingsBox laySep PackNatural 0

  (noteSettingsBox, guiCellMCBMVar) <- noteSettingsBox
  (boardCont, chanMapRV, _{-curPageRV-}) <- createNotebook addLayerRV rmLayerRV
                                       layerMCBMVar guiCellMCBMVar
  boxPackStart mainBox boardCont PackNatural 0

  --handleSaveLoad tempoRV boardRV layerRV instrRV pieceArrRV confSaveRV confLoadRV
{-
  boardRunRV <- newCBMVarRW BoardStop
  reactiveValueOnCanRead playRV $ reactiveValueWrite boardRunRV BoardStart
  reactiveValueOnCanRead stopRV $ reactiveValueWrite boardRunRV BoardStop
  board <- reactiveValueRead boardRV
  layer <- reactiveValueRead layerRV
  tempo <- reactiveValueRead tempoRV
  (inBoard, outBoard) <- yampaReactiveDual (board, layer, tempo, BoardStop) boardSF
  let tempoRV' = liftR2 (\bool t -> t * fromEnum (not bool)) pauseRV tempoRV
      inRV = liftR4 (,,,)
             boardRV layerRV tempoRV' boardRunRV
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
-}
  widgetShowAll window
  ------------------------------------------------------------

  boxPackStart settingsBox noteSettingsBox PackNatural 10
  onDestroy window mainQuit
  mainGUI
