{-# LANGUAGE OverloadedStrings #-}

module RMCA.GUI.Buttons where

import Data.ReactiveValue
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Reactive
import System.Glib

gtkMediaPlay :: DefaultGlibString
gtkMediaPlay = stringToGlib "gtk-media-play"

gtkMediaStop :: DefaultGlibString
gtkMediaStop = stringToGlib "gtk-media-stop"

gtkMediaPause :: DefaultGlibString
gtkMediaPause = stringToGlib "gtk-media-pause"

gtkMediaRecord :: DefaultGlibString
gtkMediaRecord = stringToGlib "gtk-media-record"

gtkMediaSave :: DefaultGlibString
gtkMediaSave = stringToGlib "gtk-save"

gtkMediaOpen :: DefaultGlibString
gtkMediaOpen = stringToGlib "gtk-open"

buttonNewFromStockWithLabel :: StockId -> String -> IO Button
buttonNewFromStockWithLabel s l = do
  button <- buttonNew
  buttonBox <- hBoxNew False 0
  buttonImg <- imageNewFromStock s IconSizeButton
  buttonLabel <- labelNew (Just l)
  labelSetUseUnderline buttonLabel True
  containerAdd button buttonBox
  boxPackStart buttonBox buttonImg PackRepel 0
  boxPackStart buttonBox buttonLabel PackRepel 0
  return button

toggleButtonNewFromStock :: StockId -> IO ToggleButton
toggleButtonNewFromStock s = do
  button <- toggleButtonNew
  buttonBox <- hBoxNew False 0
  buttonImg <- imageNewFromStock s IconSizeButton
  stockTxt <- stockLookupItem s
  buttonLabel <- labelNew (siLabel <$> stockTxt)
  labelSetUseUnderline buttonLabel True
  containerAdd button buttonBox
  boxPackStart buttonBox buttonImg PackRepel 0
  boxPackStart buttonBox buttonLabel PackRepel 0
  return button

getButtons :: IO ( VBox
                 , ReactiveFieldRead IO ()
                 , ReactiveFieldRead IO ()
                 , ReactiveFieldRead IO Bool
                 , ReactiveFieldRead IO Bool
                 , ReactiveFieldRead IO ()
                 , ReactiveFieldRead IO ()
                 )
getButtons = do
  buttonBox <- vBoxNew False 10

  buttonBoxTop <- hBoxNew True 10
  boxPackStart buttonBox buttonBoxTop PackNatural 0

  buttonSave <- buttonNewFromStockWithLabel gtkMediaSave "_Save configuration"
  let confSaveRV = buttonActivateField buttonSave
  boxPackStart buttonBoxTop buttonSave PackGrow 0

  buttonLoad <- buttonNewFromStockWithLabel gtkMediaOpen "_Load configuration"
  let confLoadRV = buttonActivateField buttonLoad
  boxPackStart buttonBoxTop buttonLoad PackGrow 0


  buttonBoxBot <- hBoxNew True 10
  boxPackStart buttonBox buttonBoxBot PackNatural 0
  buttonPlay <- buttonNewFromStock gtkMediaPlay
  let playRV = buttonActivateField buttonPlay
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
         )
