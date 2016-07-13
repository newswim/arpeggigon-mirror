{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TupleSections #-}

module RMCA.GUI.LayerSettings where

import Data.ReactiveValue
import Data.String
import Data.Tuple
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Reactive
import RMCA.Auxiliary.RV
import RMCA.GUI.NoteSettings
import RMCA.Layer.Layer
import RMCA.Semantics
import RMCA.Translator.Instruments
import RMCA.Translator.Message

floatConv :: (ReactiveValueReadWrite a b m,
              Real c, Real b, Fractional c, Fractional b) =>
             a -> ReactiveFieldReadWrite m c
floatConv = liftRW $ bijection (realToFrac, realToFrac)

layerSettings :: ( ReactiveValueReadWrite board ([Note],[Message]) IO
                 , ReactiveValueRead chan Int IO) =>
                 chan -> board -> IO (VBox, ReactiveFieldReadWrite IO Layer)
layerSettings chanRV boardQueue = do
  layerSettingsVBox <- vBoxNew True 10
  layerSettingsBox <- hBoxNew True 10
  boxPackStart layerSettingsVBox layerSettingsBox PackNatural 0

  layTempoBox <- hBoxNew False 10
  boxPackStart layerSettingsBox layTempoBox PackNatural 0
  layTempoLabel <- labelNew (Just "Layer tempo")
  labelSetAngle layTempoLabel 90
  boxPackStart layTempoBox layTempoLabel PackNatural 0
  layTempoAdj <- adjustmentNew 1 0 2 1 1 1
  layTempoScale <- vScaleNew layTempoAdj
  boxPackStart layTempoBox layTempoScale PackNatural 0

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

  instrumentCombo <- comboBoxNewText
  instrumentIndex <- mapM (\(ind,ins) ->
                             do i <- comboBoxAppendText instrumentCombo $
                                     fromString ins
                                return (i, ind)) instrumentList
  comboBoxSetActive instrumentCombo 0
  boxPackStart layerSettingsVBox instrumentCombo PackNatural 10
  let indexToInstr i = case (lookup i instrumentIndex) of
        Nothing -> error "Can't get the selected instrument."
        Just x -> x
      instrToIndex ins = case (lookup ins $ map swap instrumentIndex) of
        Nothing -> error "Can't retrieve the index for the instrument."
        Just x -> x
      instrumentComboRV = bijection (indexToInstr, instrToIndex) `liftRW`
                          comboBoxIndexRV instrumentCombo

  reactiveValueOnCanRead instrumentComboRV $ do
    ins <- reactiveValueRead instrumentComboRV
    chan <- reactiveValueRead chanRV
    reactiveValueAppend boardQueue ([],[Instrument (mkChannel chan) (mkProgram ins)])

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
  return (layerSettingsVBox, layerRV)
