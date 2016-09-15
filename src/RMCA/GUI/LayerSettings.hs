{-# LANGUAGE FlexibleContexts, LambdaCase, MultiParamTypeClasses, TupleSections
             #-}

module RMCA.GUI.LayerSettings where

import Data.Maybe
import Data.ReactiveValue
import Data.String
import Data.Tuple
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Reactive
import RMCA.Auxiliary
import RMCA.GUI.NoteSettings
import RMCA.Layer.Board
import RMCA.Layer.LayerConf
import RMCA.MCBMVar
import RMCA.Translator.Instruments

mkVScale :: String -> Adjustment -> IO (HBox,VScale)
mkVScale s adj = do
  hBox <- hBoxNew False 10
  boxLabel <- labelNew (Just s)
  labelSetAngle boxLabel 90
  boxPackStart hBox boxLabel PackNatural 0
  boxScale <- vScaleNew adj
  boxPackStart hBox boxScale PackNatural 0
  return (hBox,boxScale)

layerSettings :: (ReactiveValueRead isStarted RunStatus IO) =>
                 isStarted -> IO ( VBox
                                 , MCBMVar StaticLayerConf
                                 , MCBMVar DynLayerConf
                                 , MCBMVar SynthConf
                                 )
layerSettings isStartedRV = do
  ------------------------------------------------------------------------------
  -- GUI Boxes
  ------------------------------------------------------------------------------
  layerSettingsVBox <- vBoxNew True 10
  layerSettingsBox <- hBoxNew False 0
  centerSettings <- alignmentNew 0.5 0.5 0 0
  containerAdd layerSettingsBox centerSettings
  boxPackStart layerSettingsVBox layerSettingsBox PackNatural 0


  layBeatBox <- hBoxNew False 10
  layBeatCombo <- comboBoxNewText
  layBeatIndex <- mapM (\(str,dur) -> do i <- comboBoxAppendText layBeatCombo
                                              (fromString str)
                                         return (dur,i)) noteList'
  comboBoxSetActive layBeatCombo 0
  let indexToDur i =
        fromMaybe (error "In indexToDur: failed \
                         \to find the correct \
                         \ duration for the \
                         \selected index.") $ lookup i $ map swap layBeatIndex
      durToIndex d =
        fromMaybe (error "In durToIndex: \
                         \failed to find \
                         \the correct index \
                         \for the duration.") $ lookup d layBeatIndex
      layBeatRV = bijection (indexToDur, durToIndex) `liftRW`
                  comboBoxIndexRV layBeatCombo
  layBeatLabel <- labelNew (Just "Layer beat"){-=<<
                  (`lookup` symbolString) <$> reactiveValueRead layBeatRV-}
  --labelSetAngle layBeatLabel 90
  labelSetLineWrap layBeatLabel True
  --let layBeatLabelRV = labelTextReactive layBeatLabel
  boxPackStart layerSettingsBox layBeatBox PackRepel 0
  auxLayBeatBox <- vBoxNew False 0
  boxPackEnd layBeatBox auxLayBeatBox PackRepel 0
  boxPackStart auxLayBeatBox layBeatLabel PackRepel 0
  boxPackStart auxLayBeatBox layBeatCombo PackNatural 0

  layVolumeAdj <- adjustmentNew 100 0 100 1 1 1
  (layVolumeBox,layVolumeScale) <- mkVScale "Volume" layVolumeAdj
  boxPackStart layerSettingsBox layVolumeBox PackRepel 0
  (Requisition layVolW layVolH) <- widgetSizeRequest layVolumeScale
  widgetSetSizeRequest layerSettingsBox layVolW (max layVolH 75)
  scaleSetDigits layVolumeScale 0
  {-
  layTempoAdj <- adjustmentNew 1 0 2 0.1 0.1 1
  (layTempoBox, layTempoScale) <- mkVScale "Layer tempo" layTempoAdj
  boxPackStart layerSettingsBox layTempoBox PackNatural 0
-}
  strAdj <- adjustmentNew 0.8 0 2 0.1 0.1 0
  (strBox, layStrengthScale) <- mkVScale "Strength" strAdj
  boxPackStart layerSettingsBox strBox PackRepel 0

  layerSettingsBox' <- hBoxNew False 10
  boxPackStart layerSettingsVBox layerSettingsBox' PackNatural 0
  centerSettings' <- alignmentNew 0 0.5 0 0
  containerAdd layerSettingsBox' centerSettings'

  bpbBox <- vBoxNew False 0
  boxPackStart layerSettingsBox' bpbBox PackRepel 0
  bpbLabel <- labelNew (Just "Beats per bar")
  labelSetLineWrap bpbLabel True
  bpbAdj <- adjustmentNew 4 1 16 1 1 0
  bpbButton <- spinButtonNew bpbAdj 1 0
  auxBpbBox <- vBoxNew False 0
  centerAl <- alignmentNew 0.5 0.5 0 0
  containerAdd auxBpbBox centerAl
  boxPackStart bpbBox auxBpbBox PackRepel 0
  boxPackStart auxBpbBox bpbLabel PackGrow 0
  boxPackStart auxBpbBox bpbButton PackGrow 0

  repeatBox <- vBoxNew False 0
  boxPackStart layerSettingsBox' repeatBox PackRepel 0
  repeatLabel <- labelNew (Just "Repeat count")
  labelSetLineWrap repeatLabel True
  repeatAdj <- adjustmentNew 0 0 (fromIntegral (maxBound :: Int)) 1 1 0
  repeatButton <- spinButtonNew repeatAdj 1 0
  auxRepeatBox <- vBoxNew False 0
  centerAl' <- alignmentNew 0.5 0.5 0 0
  containerAdd auxRepeatBox centerAl'
  boxPackStart repeatBox auxRepeatBox PackRepel 0
  boxPackStart auxRepeatBox repeatLabel PackGrow 0
  boxPackStart auxRepeatBox repeatButton PackGrow 0
  repeatCheckButton <- checkButtonNewWithLabel "Unable repeat count"
  boxPackStart auxRepeatBox repeatCheckButton PackGrow 0

  instrumentCombo <- comboBoxNewText
  instrumentIndex <- mapM (\(ind,ins) ->
                             do i <- comboBoxAppendText instrumentCombo $
                                     fromString ins
                                return (i, ind)) instrumentList
  comboBoxSetActive instrumentCombo 0
  boxPackStart layerSettingsVBox instrumentCombo PackNatural 10
  ------------------------------------------------------------------------------
  -- RVs
  ------------------------------------------------------------------------------
  let indexToInstr i = fromMaybe (error "Can't get the selected instrument.") $
                       lookup i instrumentIndex
      instrToIndex ins =
        fromMaybe (error "Can't retrieve the index for the instrument.") $
        lookup ins $ map swap instrumentIndex
      instrumentComboRV = bijection (indexToInstr, instrToIndex) `liftRW`
                          comboBoxIndexRV instrumentCombo
      layVolumeRV = liftRW (bijection (floor, fromIntegral)) $
                    scaleValueReactive layVolumeScale

  synthMCBMVar <- newMCBMVar
    =<< reactiveValueRead (liftR2 SynthConf layVolumeRV instrumentComboRV)

  layPitchRV <- newCBMVarRW 1
  let strengthRV = floatConv $ scaleValueReactive layStrengthScale

  dynMCBMVar <- newMCBMVar
    =<< reactiveValueRead (liftR3 DynLayerConf layBeatRV layPitchRV strengthRV)

  let bpbRV = spinButtonValueIntReactive bpbButton
      repeatCheckRV = toggleButtonActiveReactive repeatCheckButton
      repeatRV' = spinButtonValueIntReactive repeatButton
      repeatRV = let f (act,r) = if act then Just r else Nothing
                     g r = case r of
                       Nothing -> (False,0)
                       Just n  -> (True,n)
                 in liftRW2 (bijection (g,f)) repeatCheckRV repeatRV'
      repeatSensitive = widgetSensitiveReactive repeatButton
      repeatCheckSensitive = widgetSensitiveReactive repeatCheckButton
      bpbSensitiveRV = widgetSensitiveReactive bpbButton

  reactiveValueOnCanRead isStartedRV $
    reactiveValueRead isStartedRV >>=
    \case
      Running ->  do reactiveValueRead repeatCheckRV
                     reactiveValueWrite repeatSensitive False
                     reactiveValueWrite bpbSensitiveRV False
                     reactiveValueWrite repeatCheckSensitive False
      Stopped -> do reactiveValueRead repeatCheckRV >>=
                      reactiveValueWrite repeatSensitive
                    reactiveValueWrite bpbSensitiveRV True
                    reactiveValueWrite repeatCheckSensitive True

  repeatCheckRV =:> repeatSensitive
  reactiveValueWrite repeatCheckRV False
  reactiveValueWrite repeatSensitive False

  statMCBMVar <- newMCBMVar
    =<< reactiveValueRead (liftR2 StaticLayerConf bpbRV repeatRV)

  reactiveValueOnCanRead dynMCBMVar $ postGUIAsync $ do
    nDyn <- reactiveValueRead dynMCBMVar
    reactiveValueWriteOnNotEq layBeatRV $ layerBeat nDyn
    reactiveValueWriteOnNotEq layPitchRV $ relPitch nDyn
    reactiveValueWriteOnNotEq strengthRV $ strength nDyn

  reactiveValueOnCanRead statMCBMVar $ postGUIAsync $ do
    nStat <- reactiveValueRead statMCBMVar
    reactiveValueWriteOnNotEq bpbRV $ beatsPerBar nStat
    reactiveValueWriteOnNotEq repeatRV $ repeatCount nStat

  reactiveValueOnCanRead synthMCBMVar $ do
    nSynth <- reactiveValueRead synthMCBMVar
    reactiveValueWriteOnNotEq layVolumeRV $ volume nSynth
    reactiveValueWriteOnNotEq instrumentComboRV $ instrument nSynth

  syncRightOnLeftWithBoth (\nt ol -> ol { layerBeat = nt })
    layBeatRV dynMCBMVar
  syncRightOnLeftWithBoth (\np ol -> ol { relPitch = np })
    layPitchRV dynMCBMVar
  syncRightOnLeftWithBoth (\ns ol -> ol { strength = ns })
    strengthRV dynMCBMVar
  syncRightOnLeftWithBoth (\nb ol -> ol { beatsPerBar = nb })
    bpbRV statMCBMVar
  syncRightOnLeftWithBoth (\nr ol -> ol { repeatCount = nr })
    repeatRV statMCBMVar
  syncRightOnLeftWithBoth (\nv ol -> ol { volume = nv })
    layVolumeRV synthMCBMVar
  syncRightOnLeftWithBoth (\ni ol -> ol { instrument = ni })
    instrumentComboRV synthMCBMVar

  return (layerSettingsVBox, statMCBMVar, dynMCBMVar, synthMCBMVar)
