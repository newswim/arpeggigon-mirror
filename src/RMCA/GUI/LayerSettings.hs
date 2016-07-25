{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TupleSections #-}

module RMCA.GUI.LayerSettings where

import Data.Maybe
import Data.ReactiveValue
import Data.String
import Data.Tuple
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Reactive
import RMCA.Auxiliary
import RMCA.GUI.NoteSettings
import RMCA.Layer.Layer
import RMCA.Semantics
import RMCA.Translator.Instruments
import RMCA.Translator.Message

floatConv :: (ReactiveValueReadWrite a b m,
              Real c, Real b, Fractional c, Fractional b) =>
             a -> ReactiveFieldReadWrite m c
floatConv = liftRW $ bijection (realToFrac, realToFrac)

mkVScale :: String -> Adjustment -> IO (HBox,VScale)
mkVScale s adj = do
  hBox <- hBoxNew False 10
  boxLabel <- labelNew (Just s)
  labelSetAngle boxLabel 90
  boxPackStart hBox boxLabel PackNatural 0
  boxScale <- vScaleNew adj
  boxPackStart hBox boxScale PackNatural 0
  return (hBox,boxScale)

layerSettings :: ( ReactiveValueReadWrite board ([Note],[Message]) IO
                 , ReactiveValueRead chan Int IO) =>
                 chan -> board
              -> IO ( VBox
                    , ReactiveFieldReadWrite IO Layer
                    , ReactiveFieldReadWrite IO Int
                    )
layerSettings chanRV boardQueue = do
  layerSettingsVBox <- vBoxNew False 10
  layerSettingsBox <- hBoxNew True 10
  boxPackStart layerSettingsVBox layerSettingsBox PackNatural 0

  layVolumeAdj <- adjustmentNew 100 0 100 1 1 1
  (layVolumeBox,layVolumeScale) <- mkVScale "Volume" layVolumeAdj
  boxPackStart layerSettingsBox layVolumeBox PackNatural 0
  scaleSetDigits layVolumeScale 0


  layTempoAdj <- adjustmentNew 1 0 2 0.1 0.1 1
  (layTempoBox, layTempoScale) <- mkVScale "Layer tempo" layTempoAdj
  boxPackStart layerSettingsBox layTempoBox PackNatural 0

  strAdj <- adjustmentNew 0.8 0 2 0.1 0.1 0
  (strBox, layStrengthScale) <- mkVScale "Strength" strAdj
  boxPackStart layerSettingsBox strBox PackNatural 0

  bpbBox <- vBoxNew False 10
  boxPackStart layerSettingsBox bpbBox PackNatural 0
  bpbLabel <- labelNew (Just "Beat per bar")
  labelSetLineWrap bpbLabel True
  boxPackStart bpbBox bpbLabel PackNatural 0
  bpbAdj <- adjustmentNew 4 1 16 1 1 0
  bpbButton <- spinButtonNew bpbAdj 1 0
  boxPackStart bpbBox bpbButton PackNatural 0

  instrumentCombo <- comboBoxNewText
  instrumentIndex <- mapM (\(ind,ins) ->
                             do i <- comboBoxAppendText instrumentCombo $
                                     fromString ins
                                return (i, ind)) instrumentList
  comboBoxSetActive instrumentCombo 0
  boxPackStart layerSettingsVBox instrumentCombo PackNatural 10
  let indexToInstr i = fromMaybe (error "Can't get the selected instrument.") $
                       lookup i instrumentIndex
      instrToIndex ins =
        fromMaybe (error "Can't retrieve the index for the instrument.") $
        lookup ins $ map swap instrumentIndex
      instrumentComboRV = bijection (indexToInstr, instrToIndex) `liftRW`
                          comboBoxIndexRV instrumentCombo

      changeInst = do
        ins <- reactiveValueRead instrumentComboRV
        chan <- reactiveValueRead chanRV
        reactiveValueAppend boardQueue
          ([],[Instrument (mkChannel chan) (mkProgram ins)])
  changeInst
  reactiveValueOnCanRead instrumentComboRV changeInst

  layPitchRV <- newCBMVarRW 1
  let layTempoRV = floatConv $ scaleValueReactive layTempoScale
      strengthRV = floatConv $  scaleValueReactive layStrengthScale
      bpbRV = spinButtonValueIntReactive bpbButton
      layVolumeRV = liftRW (bijection (floor, fromIntegral)) $
                    scaleValueReactive layVolumeScale
      f1 Layer { relTempo = d
               , relPitch = p
               , strength = s
               , beatsPerBar = bpb
               , volume = v
               } = (d,p,s,bpb,v)
      f2 (d,p,s,bpb,v) = Layer { relTempo = d
                               , relPitch = p
                               , strength = s
                               , beatsPerBar = bpb
                               , volume = v
                               }
      layerRV = liftRW5 (bijection (f1,f2))
                layTempoRV layPitchRV strengthRV bpbRV layVolumeRV

  reactiveValueOnCanRead layVolumeRV $ do
    vol <- reactiveValueRead layVolumeRV
    chan <- reactiveValueRead chanRV
    let vol' = floor ((fromIntegral vol / 100) * 127)
    reactiveValueAppend boardQueue ([],[Volume (mkChannel chan) vol'])
  return (layerSettingsVBox, layerRV, instrumentComboRV)
