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

toggleButtonNewFromStock :: StockId -> IO ToggleButton
toggleButtonNewFromStock s = do
  button <- toggleButtonNew
  buttonBox <- hBoxNew False 0
  buttonImg <- imageNewFromStock s IconSizeButton
  stockTxt <- stockLookupItem s
  buttonLabel <- labelNew (siLabel <$> stockTxt)
  labelSetUseUnderline buttonLabel True
  containerAdd button buttonBox
  boxPackStart buttonBox buttonImg PackNatural 0
  boxPackStart buttonBox buttonLabel PackNatural 0
  return button

getButtons :: IO ( HBox
                 , ReactiveFieldRead IO ()
                 , ReactiveFieldRead IO ()
                 , ReactiveFieldRead IO Bool
                 , ReactiveFieldRead IO Bool
                 )
getButtons = do
  buttonBox <- hBoxNew True 10
  buttonPlay <- buttonNewFromStock gtkMediaPlay
  let playRV = buttonActivateField buttonPlay
  boxPackStart buttonBox buttonPlay PackRepel 0

  buttonPause <- toggleButtonNewFromStock gtkMediaPause
  let pauseRV = readOnly $ toggleButtonActiveReactive buttonPause
  boxPackStart buttonBox buttonPause PackRepel 0

  buttonStop <- buttonNewFromStock gtkMediaStop
  let stopRV = buttonActivateField buttonStop
  boxPackStart buttonBox buttonStop PackRepel 0

  buttonRecord <- toggleButtonNewFromStock gtkMediaRecord
  let recordRV = readOnly $ toggleButtonActiveReactive buttonRecord
  boxPackStart buttonBox buttonRecord PackRepel 0

  return ( buttonBox
         , playRV
         , stopRV
         , pauseRV
         , recordRV
         )
