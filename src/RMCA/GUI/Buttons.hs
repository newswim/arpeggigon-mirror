{-# LANGUAGE FlexibleContexts, LambdaCase, MultiParamTypeClasses,
             OverloadedStrings #-}

module RMCA.GUI.Buttons ( buttonNewFromStockWithLabel
                        , toggleButtonNewFromStock
                        , getButtons
                        ) where

import Control.Monad
import Data.Maybe
import Data.ReactiveValue
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Reactive
import RMCA.GUI.StockId
import RMCA.Layer.Board

packButton :: (BoxClass a, ButtonClass b, ImageClass i, LabelClass l) =>
              b -> a -> l -> i -> IO b
packButton button buttonBox buttonLabel buttonImg = do
  containerAdd button buttonBox
  boxPackStart buttonBox buttonImg PackRepel 0
  boxPackStart buttonBox buttonLabel PackRepel 0
  return button

buttonNewFromStockWithLabel :: StockId -> String -> IO Button
buttonNewFromStockWithLabel s l = do
  button <- buttonNew
  buttonBox <- hBoxNew False 0
  buttonImg <- imageNewFromStock s IconSizeButton
  buttonLabel <- labelNew (Just l)
  labelSetUseUnderline buttonLabel True
  packButton button buttonBox buttonLabel buttonImg

toggleButtonNewFromStock :: StockId -> IO ToggleButton
toggleButtonNewFromStock s = do
  button <- toggleButtonNew
  buttonBox <- hBoxNew False 0
  buttonImg <- imageNewFromStock s IconSizeButton
  stockTxt <- stockLookupItem s
  buttonLabel <- labelNew (fmap siLabel stockTxt)
  labelSetUseUnderline buttonLabel True
  packButton button buttonBox buttonLabel buttonImg

getButtons :: (ReactiveValueRead boardStatus RunStatus IO) =>
              boardStatus -> IO ( VBox
                                , ReactiveFieldRead IO ()
                                , ReactiveFieldRead IO ()
                                , ReactiveFieldRead IO Bool
                                , ReactiveFieldRead IO Bool
                                , ReactiveFieldRead IO ()
                                , ReactiveFieldRead IO ()
                                , ReactiveFieldRead IO ()
                                , ReactiveFieldRead IO ()
                                , ReactiveFieldRead IO ()
                                )
getButtons boardStatusRV = do
  --addRestartButton
  restartM <- stockLookupItem gtkMediaRestart
  when (isJust restartM) $
    stockAddItem [(fromJust restartM) { siLabel = "_Restart" }]
  buttonBox <- vBoxNew False 5

  buttonBoxAddRmLayers <- hBoxNew True 10
  boxPackStart buttonBox buttonBoxAddRmLayers PackNatural 0

  buttonAddLayer <- buttonNewFromStockWithLabel gtkMediaAdd "Add layer"
  let addLayerRV = buttonActivateField buttonAddLayer
  boxPackStart buttonBoxAddRmLayers buttonAddLayer PackGrow 0

  buttonRmLayer <- buttonNewFromStockWithLabel gtkMediaRemove "Remove layer"
  let rmLayerRV = buttonActivateField buttonRmLayer
  boxPackStart buttonBoxAddRmLayers buttonRmLayer PackGrow 0

  buttonRmAll <- buttonNewFromStockWithLabel gtkMediaRemove "Clear"
  let rmAllRV = buttonActivateField buttonRmAll
  boxPackStart buttonBoxAddRmLayers buttonRmAll PackGrow 0

  buttonBoxSaveLoad <- hBoxNew True 10
  boxPackStart buttonBox buttonBoxSaveLoad PackNatural 0

  buttonSave <- buttonNewFromStockWithLabel gtkMediaSave "_Save configuration"
  let confSaveRV = buttonActivateField buttonSave
  boxPackStart buttonBoxSaveLoad buttonSave PackGrow 0

  buttonLoad <- buttonNewFromStockWithLabel gtkMediaOpen "_Load configuration"
  let confLoadRV = buttonActivateField buttonLoad
  boxPackStart buttonBoxSaveLoad buttonLoad PackGrow 0

  buttonBoxBot <- hBoxNew True 10
  boxPackStart buttonBox buttonBoxBot PackNatural 0
  buttonPlay <- buttonNewFromStock gtkMediaPlay
  let playRV = buttonActivateField buttonPlay
      playStockId = wrapMW (buttonSetLabel buttonPlay)
  reactiveValueWrite playStockId gtkMediaPlay
  reactiveValueOnCanRead boardStatusRV $ reactiveValueRead boardStatusRV >>=
    \case
      Stopped -> reactiveValueWrite playStockId gtkMediaPlay
      Running -> reactiveValueWrite playStockId gtkMediaRestart
  boxPackStart buttonBoxBot buttonPlay PackRepel 0

  buttonPause <- toggleButtonNewFromStock gtkMediaPause
  let pauseRV = readOnly $ toggleButtonActiveReactive buttonPause
  boxPackStart buttonBoxBot buttonPause PackRepel 0

  buttonStop <- buttonNewFromStock gtkMediaStop
  let stopRV = buttonActivateField buttonStop
  boxPackStart buttonBoxBot buttonStop PackRepel 0

  buttonRecord <- toggleButtonNewFromStock gtkMediaRecord
  let recordRV = readOnly $ toggleButtonActiveReactive buttonRecord
  boxPackStart buttonBoxBot buttonRecord PackRepel 0

  return ( buttonBox
         , playRV
         , stopRV
         , pauseRV
         , recordRV
         , confSaveRV
         , confLoadRV
         , addLayerRV
         , rmLayerRV
         , rmAllRV
         )
