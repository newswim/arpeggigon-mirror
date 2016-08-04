{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables,
             TupleSections #-}

module RMCA.GUI.NoteSettings where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Ord
import Data.Ratio
import Data.ReactiveValue
import Data.String
import Data.Tuple
import Graphics.UI.Gtk          hiding (Action)
import Graphics.UI.Gtk.Reactive
import RMCA.Auxiliary
import RMCA.GUI.Board
import RMCA.MCBMVar
import RMCA.Semantics

setNAttr :: NoteAttr -> Action -> Action
setNAttr _ Inert = Inert
setNAttr _ Absorb = Absorb
setNAttr na (Stop _) = Stop na
setNAttr na (ChDir b _ dir) = ChDir b na dir
setNAttr na (Split _) = Split na

getNAttr :: Action -> Maybe NoteAttr
getNAttr Inert = Nothing
getNAttr Absorb = Nothing
getNAttr (Stop na) = Just na
getNAttr (ChDir _ na _) = Just na
getNAttr (Split na) = Just na

symbolString :: [(Duration,String)]
symbolString = map (\(_,y,z) -> (z,y)) noteSymbList

noteList :: [(String,Duration)]
noteList = map (\(x,_,y) -> (x,y)) noteSymbList

noteSymbList :: [(String, String, Duration)]
noteSymbList = sortBy (comparing (\(_,_,x) -> x))
               [ ("♩", "Quarter note", 1 % 4)
               , ("♪", "Eighth note ", 1 % 8)
               , ("𝅗𝅥", "Half note", 1 % 2)
               , ("𝅘𝅥𝅯", "Sixteenth note", 1 % 16)
               , ("𝅝", "Whole note", 1)
               ]

comboBoxIndexRV :: (ComboBoxClass box) =>
                   box -> ReactiveFieldReadWrite IO Int
comboBoxIndexRV box = ReactiveFieldReadWrite setter getter notifier
  where getter = comboBoxGetActive box
        setter = comboBoxSetActive box
        notifier = void . on box changed

noteSettingsBox :: IO (VBox, MCBMVar GUICell)
noteSettingsBox = do
  pieceBox <- vBoxNew False 10
  naBox <- vBoxNew False 10
  boxPackStart pieceBox naBox PackNatural 10

  -- Articulation box
  artCombo <- comboBoxNewText
  artIndex <- mapM (\art -> do i <- comboBoxAppendText artCombo
                                    (fromString $ show art)
                               return (art,i)) [NoAccent ..]
  comboBoxSetActive artCombo 0
  boxPackStart naBox artCombo PackNatural 10
  let indexToArt i =
        fromMaybe (error "In indexToArt: failed \
                         \to find the selected \
                         \articulation.") $ lookup i $ map swap artIndex
      artToIndex a = fromMaybe (error "In artToIndex: failed \
                                      \to find the correct index \
                                      \for the \
                                      \articulation.") $ lookup a artIndex
      artComboRV = bijection (indexToArt,artToIndex) `liftRW`
                   comboBoxIndexRV artCombo

  -- Slide box
  slideCombo <- comboBoxNewText
  slideIndex <- mapM (\sli -> do i <- comboBoxAppendText slideCombo
                                      (fromString $ show sli)
                                 return (sli,i)) [NoSlide ..]
  comboBoxSetActive slideCombo 0
  boxPackStart naBox slideCombo PackNatural 10
  let indexToSlide i =
        fromMaybe (error "In indexToSlide: failed \
                         \to find the correct slide \
                         \for the selected \
                         \index.") $ lookup i $ map swap slideIndex
      slideToIndex s =
        fromMaybe (error "In slideToIndex: failed \
                         \to find \
                         \the correct index \
                         \for the slide.") $ lookup s slideIndex
      slideComboRV = bijection (indexToSlide,slideToIndex) `liftRW`
                     comboBoxIndexRV slideCombo

  -- Note duration box
  noteDurBox <- hBoxNew False 10
  noteDurCombo <- comboBoxNewText
  noteDurIndex <- mapM (\(str,dur) -> do i <- comboBoxAppendText noteDurCombo
                                              (fromString str)
                                         return (dur,i)) noteList
  comboBoxSetActive noteDurCombo 0
  let indexToDur i =
        fromMaybe (error "In indexToDur: failed \
                         \to find the correct \
                         \ duration for the \
                         \selected index.") $ lookup i $ map swap noteDurIndex
      durToIndex d =
        fromMaybe (error "In durToIndex: \
                         \failed to find \
                         \the correct index \
                         \for the duration.") $ lookup d noteDurIndex
      noteDurRV = bijection (indexToDur, durToIndex) `liftRW`
                  comboBoxIndexRV noteDurCombo
  noteDurLabel <- labelNew =<< (`lookup` symbolString) <$> reactiveValueRead noteDurRV
  let noteDurLabelRV = labelTextReactive noteDurLabel
  boxPackStart naBox noteDurBox PackNatural 10
  boxPackStart noteDurBox noteDurCombo PackNatural 10
  boxPackStart noteDurBox noteDurLabel PackNatural 10

  -- Repeat count box
  rCountAdj <- adjustmentNew 1 0 100 1 1 0
  rCount <- spinButtonNew rCountAdj 1 0
  boxPackStart pieceBox rCount PackNatural 10
  let rCountRV = spinButtonValueIntReactive rCount

  -- Side RV
  -- Carries the index of the tile to display and what to display.
  setRV <- newMCBMVar inertCell

  reactiveValueOnCanRead noteDurRV $ do
    nDur <- reactiveValueRead noteDurRV
    oCell <- reactiveValueRead setRV
    let nCa :: Maybe NoteAttr
        nCa = (\na -> na { naDur = nDur }) <$> getNAttr (cellAction oCell)
        nCell :: GUICell
        nCell = if isJust nCa
                then oCell { cellAction =
                             setNAttr (fromJust nCa) (cellAction oCell) }
                else oCell
    reactiveValueWriteOnNotEq setRV nCell
    fromMaybeM_ $ reactiveValueWrite noteDurLabelRV <$> lookup nDur symbolString


  reactiveValueOnCanRead rCountRV $ do
    nRCount <- reactiveValueRead rCountRV
    oCell <- reactiveValueRead setRV
    let nCell = oCell { repeatCount = nRCount }
    reactiveValueWrite setRV nCell

  reactiveValueOnCanRead slideComboRV $ do
    nSlide <- reactiveValueRead slideComboRV
    oCell <- reactiveValueRead setRV
    let nCa :: Maybe NoteAttr
        nCa = (\na -> na { naOrn = (naOrn na) { ornSlide = nSlide } }) <$>
              getNAttr (cellAction oCell)
        nCell :: GUICell
        nCell = if isJust nCa
                then oCell { cellAction =
                             setNAttr (fromJust nCa) (cellAction oCell)
                           }
                else oCell
    reactiveValueWrite setRV nCell

  reactiveValueOnCanRead artComboRV $ do
    nArt <- reactiveValueRead artComboRV
    oCell <- reactiveValueRead setRV
    let nCa :: Maybe NoteAttr
        nCa = (\na -> na { naArt = nArt }) <$> getNAttr (cellAction oCell)
        nCell :: GUICell
        nCell = if isJust nCa
                then oCell { cellAction =
                             setNAttr (fromJust nCa) (cellAction oCell) }
                else oCell
    reactiveValueWrite setRV nCell

  let hideNa :: IO ()
      hideNa = do widgetHide slideCombo
                  widgetHide artCombo
                  widgetShow rCount
                  widgetHideAll noteDurBox
      showNa :: IO ()
      showNa = do widgetShow slideCombo
                  widgetShow artCombo
                  widgetShow rCount
                  widgetShowAll noteDurBox
      updateNaBox :: GUICell -> IO ()
      updateNaBox GUICell { cellAction = act } = case act of
        Inert -> hideNa
        Absorb -> hideNa
        _ -> showNa

  reactiveValueOnCanRead setRV $ postGUIAsync $ do
    nCell <- reactiveValueRead setRV
    fromMaybeM_ (reactiveValueWriteOnNotEq artComboRV . naArt <$>
                  getNAttr (cellAction nCell))
    fromMaybeM_ (reactiveValueWriteOnNotEq slideComboRV . ornSlide . naOrn <$>
                  getNAttr (cellAction nCell))
    reactiveValueWriteOnNotEq rCountRV $ repeatCount nCell
    fromMaybeM_ (reactiveValueWriteOnNotEq noteDurRV . naDur <$>
                  getNAttr (cellAction nCell))
    updateNaBox nCell

  widgetShow pieceBox
  widgetShow naBox
  return (pieceBox,setRV)
