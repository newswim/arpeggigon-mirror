module RMCA.GUI.MainSettings where

import Data.ReactiveValue
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Reactive

globalSettings :: IO (VBox, ReactiveFieldReadWrite IO Int)
globalSettings = do
  globalSettingsBox <- vBoxNew False 10
  tempoAdj <- adjustmentNew 120 40 200 1 1 1
  tempoLabel <- labelNew (Just "Tempo")
  boxPackStart globalSettingsBox tempoLabel PackNatural 0
  tempoScale <- hScaleNew tempoAdj
  boxPackStart globalSettingsBox tempoScale PackNatural 0
  scaleSetDigits tempoScale 0
  let tempoRV =
        bijection (floor, fromIntegral) `liftRW` scaleValueReactive tempoScale

  return (globalSettingsBox, tempoRV)
