{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables, TupleSections #-}

module Main where

import           Control.Concurrent
import qualified Data.IntMap                                as M
import           Data.ReactiveValue
import           FRP.Yampa
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.Board.BoardLink
import           Graphics.UI.Gtk.Layout.BackgroundContainer
import           Hails.Yampa
import           RMCA.Auxiliary
import           RMCA.Auxiliary
import           RMCA.Configuration
import           RMCA.GUI.Board
import           RMCA.GUI.Buttons
import           RMCA.GUI.LayerSettings
import           RMCA.GUI.MainSettings
import           RMCA.GUI.MultiBoard
import           RMCA.GUI.NoteSettings
import           RMCA.Layer.Board
import           RMCA.Layer.Layer
import           RMCA.Semantics
import           RMCA.Translator.Jack

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

  (buttonBox,
   playRV,stopRV,pauseRV,recordRV,
   confSaveRV,confLoadRV,
   addLayerRV,rmLayerRV) <- getButtons
  boxPackEnd settingsBox buttonBox PackNatural 0

  boardQueue <- newCBMVarRW mempty
  (layerSettingsVBox, layerMCBMVar, instrMCBMVar) <- layerSettings boardQueue
  boxPackStart settingsBox layerSettingsVBox PackNatural 0
  laySep <- hSeparatorNew
  boxPackStart settingsBox laySep PackNatural 0

  (noteSettingsBox, guiCellMCBMVar) <- noteSettingsBox
  (boardCont, boardMapRV, layerMapRV, phRVMapRV) <- createNotebook
                                                    addLayerRV rmLayerRV
                                                    layerMCBMVar guiCellMCBMVar
  boxPackStart mainBox boardCont PackNatural 0

  --handleSaveLoad tempoRV boardRV layerRV instrRV pieceArrRV confSaveRV confLoadRV

  boardRunRV <- newCBMVarRW BoardStop
  reactiveValueOnCanRead playRV $ reactiveValueWrite boardRunRV BoardStart
  reactiveValueOnCanRead stopRV $ reactiveValueWrite boardRunRV BoardStop
  boardMap <- reactiveValueRead boardMapRV
  layerMap <- reactiveValueRead layerMapRV
  tempo <- reactiveValueRead tempoRV
  let tempoRV' = liftR2 (\bool t -> t * fromEnum (not bool)) pauseRV tempoRV
      inRV :: ReactiveFieldRead IO (M.IntMap (Board,Layer,Tempo,BoardRun))
      inRV = liftR4 (\bm lm t br -> M.map (\(b,l) -> (b,l,t,br)) $
                      M.intersectionWith (,) bm lm)
             boardMapRV layerMapRV tempoRV' boardRunRV
  initSF <- reactiveValueRead inRV
  (inBoard, outBoard) <- yampaReactiveDual initSF (boardRun initSF)
  --reactiveValueOnCanRead inRV (reactiveValueRead inRV >>= print . M.keys)
  inRV =:> inBoard
  reactiveValueOnCanRead outBoard $ do
    out <- reactiveValueRead outBoard
    --print out
    phRVMap <- reactiveValueRead phRVMapRV

    let eventsMap = M.filter isEvent out
        writePh chan val =
          fromMaybeM_ $ fmap (\ph -> reactiveValueWrite ph val) $
          M.lookup chan phRVMap
        noteMap = M.map (eventToList . snd . splitE) out
    sequence_ $ M.mapWithKey writePh $
      M.map (fst . fromEvent) $ M.filter isEvent out
    reactiveValueAppend boardQueue $ M.map (,[]) noteMap


{-
    reactiveValueRead (liftR (event mempty (,[]) <<< snd <<< splitE) outBoard) >>=
      reactiveValueAppend boardQueue-}
  -- This needs to be set last otherwise phRV is written to, so
  -- inBoard is written to and the notes don't get played. There
  -- supposedly is no guaranty of order but apparently there is…
  putStrLn "Board started."
  -- Jack setup
  forkIO $ jackSetup boardQueue

  widgetShowAll window
  ------------------------------------------------------------

  boxPackStart settingsBox noteSettingsBox PackNatural 10
  onDestroy window mainQuit
  mainGUI
