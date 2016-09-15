{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables, TupleSections #-}

module Main where

import           Control.Concurrent
import qualified Data.IntMap                    as M
import           Data.ReactiveValue
import           FRP.Yampa
import           Graphics.UI.Gtk
import           RMCA.Auxiliary
--import           RMCA.Configuration
import           Data.CBRef
import           RMCA.EventProvider
import           RMCA.GUI.Buttons
import           RMCA.GUI.LayerSettings
import           RMCA.GUI.MainSettings
import           RMCA.GUI.MultiBoard
import           RMCA.GUI.NoteSettings
import           RMCA.IOClockworks
import           RMCA.Layer.Board
import           RMCA.ReactiveValueAtomicUpdate
import           RMCA.Translator.Jack
import           RMCA.YampaReactive

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
  boxPackEnd mainBox settingsBox PackGrow 0
  (globalSettingsBox, tempoRV) <- globalSettings
  boxPackStart settingsBox globalSettingsBox PackNatural 0
  globalSep <- hSeparatorNew
  boxPackStart settingsBox globalSep PackNatural 10

  boardStatusRV <- newCBMVarRW Stopped

  (buttonBox,
   playRV,stopRV,pauseRV,recordRV,
   confSaveRV,confLoadRV,
   addLayerRV,rmLayerRV) <- getButtons boardStatusRV
  boxPackEnd settingsBox buttonBox PackNatural 0

  boardQueue <- newCBRef mempty
  --isStartMVar <- newMVar False
  (layerSettingsVBox, statMCBMVar, dynMCBMVar, synthMCBMVar) <- layerSettings boardStatusRV
  boxPackStart settingsBox layerSettingsVBox PackNatural 0
  laySep <- hSeparatorNew
  boxPackStart settingsBox laySep PackNatural 0

  (noteSettingsBox, guiCellMCBMVar) <- noteSettingsBox
  tc <- newIOTick
  (boardCont, boardMapRV, layerMapRV, phRVMapRV) <-
    createNotebook boardQueue tc addLayerRV rmLayerRV
    statMCBMVar dynMCBMVar synthMCBMVar guiCellMCBMVar
  boxPackStart mainBox boardCont PackNatural 0

  --handleSaveLoad tempoRV boardMapRV layerMapRV instrMapRV phRVMapRV
    --addLayerRV rmLayerRV confSaveRV confLoadRV

  {-
  reactiveValueOnCanRead boardStatusRV $ do
    bs <- reactiveValueRead boardStatusRV
    case bs of
      Running -> reactiveValueWrite statConfSensitiveRV False
      Stopped -> reactiveValueWrite statConfSensitiveRV True
-}
  boardStatusEP <- getEPfromRV boardStatusRV
  reactiveValueOnCanRead playRV $ reactiveValueWrite boardStatusRV Running
  reactiveValueOnCanRead stopRV $ reactiveValueWrite boardStatusRV Stopped
  let tempoRV' = liftR2 (\bool t -> t * fromEnum (not bool)) pauseRV tempoRV
      jointedMapRV = liftR (fmap (\(x,y) -> (x,y,NoEvent))) $
                     liftR2 (M.intersectionWith (,)) boardMapRV layerMapRV
      inRV = liftR3 (,,) tempoRV' boardStatusEP jointedMapRV
  initSig <- reactiveValueRead layerMapRV
  --(inBoard, outBoard) <- yampaReactiveDual initSig (boardRun
    --initSig)
  outBoard <- yampaReactiveFrom (layers initSig) inRV
  --reactiveValueOnCanRead inRV (reactiveValueRead inRV >>= print . M.keys)
  reactiveValueOnCanRead outBoard $ do
    out <- reactiveValueRead outBoard
    --print out
    phRVMap <- reactiveValueRead phRVMapRV

    let noteMap = M.map fromEvent $ M.filter isEvent $ M.map fst out
        writePh chan val =
          fromMaybeM_ $ (`reactiveValueWrite` val) <$>
          M.lookup chan phRVMap
    sequence_ $ M.mapWithKey writePh $ M.map snd out
    reactiveValueAppend boardQueue $ M.map (,[]) noteMap

  putStrLn "Board started."

  forkIO $ jackSetup tc boardQueue tempoRV layerMapRV

  widgetShowAll window
  ------------------------------------------------------------

  boxPackStart settingsBox noteSettingsBox PackNatural 10
  onDestroy window mainQuit
  mainGUI
