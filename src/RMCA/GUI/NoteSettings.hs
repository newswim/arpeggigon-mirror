{-# LANGUAGE ScopedTypeVariables, TupleSections #-}

module RMCA.GUI.NoteSettings where

import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Array
import qualified Data.Bifunctor                   as BF
import           Data.List
import           Data.Maybe
import           Data.Ord
import           Data.Ratio
import           Data.ReactiveValue
import           Data.String
import           Data.Tuple
import           Graphics.UI.Gtk                  hiding (Action)
import           Graphics.UI.Gtk.Board.TiledBoard hiding (Board)
import           Graphics.UI.Gtk.Reactive
import           RMCA.Auxiliary.RV
import           RMCA.GUI.Board
import           RMCA.Semantics

fromMaybeM_ :: (Monad m) => Maybe (m ()) -> m ()
fromMaybeM_ = fromMaybe (return ())

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

noteList :: [(String, Duration)]
noteList = sortBy (comparing snd)
           [ ("♩ quarter note", 1 % 4)
           , ("♪ eighth note", 1 % 8)
           , ("𝅗𝅥 half note", 1 % 2)
           , ("𝅘𝅥𝅯 sixteenth note", 1 % 16)
           , ("𝅝 whole note", 1)
           ]

comboBoxIndexRV :: (ComboBoxClass box) =>
                   box -> ReactiveFieldReadWrite IO Int
comboBoxIndexRV box = ReactiveFieldReadWrite setter getter notifier
  where getter = comboBoxGetActive box
        setter = comboBoxSetActive box
        notifier = void . on box changed

clickHandling :: Array Pos (ReactiveFieldWrite IO GUICell)
              -> IOBoard -> VBox -> IO VBox
clickHandling pieceArrRV board pieceBox = do
  naBox <- vBoxNew False 10
  boxPackStart pieceBox naBox PackNatural 10

  -- Articulation box
  artCombo <- comboBoxNewText
  artIndex <- mapM (\art -> do i <- comboBoxAppendText artCombo
                                    (fromString $ show art)
                               return (art,i)) [NoAccent ..]
  comboBoxSetActive artCombo 0
  boxPackStart naBox artCombo PackNatural 10
  let indexToArt i = fromMaybe NoAccent $ lookup i $ map swap artIndex
      artToIndex a = fromMaybe (-1) $ lookup a artIndex
      artComboRV = bijection (indexToArt,artToIndex) `liftRW`
                   comboBoxIndexRV artCombo

  -- Slide box
  slideCombo <- comboBoxNewText
  slideIndex <- mapM (\sli -> do i <- comboBoxAppendText slideCombo
                                      (fromString $ show sli)
                                 return (sli,i)) [NoSlide ..]
  comboBoxSetActive slideCombo 0
  boxPackStart naBox slideCombo PackNatural 10
  let indexToSlide i = fromMaybe NoSlide $ lookup i $ map swap slideIndex
      slideToIndex s = fromMaybe (-1) $ lookup s slideIndex
      slideComboRV = bijection (indexToSlide,slideToIndex) `liftRW`
                     comboBoxIndexRV slideCombo

  -- Note duration box
  noteDurCombo <- comboBoxNewText
  noteDurIndex <- mapM (\(str,dur) -> do i <- comboBoxAppendText noteDurCombo
                                              (fromString str)
                                         return (dur,i)) noteList
  comboBoxSetActive noteDurCombo 0
  boxPackStart naBox noteDurCombo PackNatural 10
  let indexToDur i = fromMaybe (1 % 4) $ lookup i $ map swap noteDurIndex
      durToIndex d = fromMaybe 0 $ lookup d noteDurIndex
      noteDurRV = bijection (indexToDur, durToIndex) `liftRW`
                  comboBoxIndexRV noteDurCombo

  -- Repeat count box
  rCountAdj <- adjustmentNew 1 0 10 1 1 0
  rCount <- spinButtonNew rCountAdj 1 0
  boxPackStart pieceBox rCount PackNatural 10
  let rCountRV = spinButtonValueIntReactive rCount

  -- Side RV
  -- Carries the index of the tile to display and what to display.
  setRV <- newCBMVarRW ((0,0),inertCell)

  reactiveValueOnCanRead noteDurRV $ do
    nDur <- reactiveValueRead noteDurRV
    (i,oCell) <- reactiveValueRead setRV
    let nCa :: Maybe NoteAttr
        nCa = (\na -> na { naDur = nDur }) <$> getNAttr (cellAction oCell)
        nCell :: GUICell
        nCell = if isJust nCa
                then oCell { cellAction =
                             setNAttr (fromJust nCa) (cellAction oCell) }
                else oCell
    reactiveValueWrite setRV (i,nCell)
    reactiveValueWrite (pieceArrRV ! i) nCell


  reactiveValueOnCanRead rCountRV $ do
    nRCount <- reactiveValueRead rCountRV
    (i,oCell) <- reactiveValueRead setRV
    let nCell = oCell { repeatCount = nRCount }
    reactiveValueWrite setRV (i,nCell)
    reactiveValueWrite (pieceArrRV ! i) nCell

  reactiveValueOnCanRead slideComboRV $ do
    nSlide <- reactiveValueRead slideComboRV
    (i,oCell) <- reactiveValueRead setRV
    let nCa :: Maybe NoteAttr
        nCa = (\na -> na { naOrn = (naOrn na) { ornSlide = nSlide } }) <$>
              getNAttr (cellAction oCell)
        nCell :: GUICell
        nCell = if isJust nCa
                then oCell { cellAction =
                             setNAttr (fromJust nCa) (cellAction oCell)
                           }
                else oCell
    reactiveValueWrite setRV (i,nCell)
    reactiveValueWrite (pieceArrRV ! i) nCell

  reactiveValueOnCanRead artComboRV $ do
    --nArt <- reactiveValueRead artComboRV
    (i,oCell) <- reactiveValueRead setRV
    let nCa :: Maybe NoteAttr
        nCa = getNAttr $ cellAction oCell
        nCell :: GUICell
        nCell = if isJust nCa
                then oCell { cellAction =
                             setNAttr (fromJust nCa) (cellAction oCell) }
                else oCell
    reactiveValueWrite setRV (i,nCell)
    reactiveValueWrite (pieceArrRV ! i) nCell

  let hideNa :: IO ()
      hideNa = do widgetHide slideCombo
                  widgetHide artCombo
                  widgetShow rCount
                  widgetHide noteDurCombo
      showNa :: IO ()
      showNa = do widgetShow slideCombo
                  widgetShow artCombo
                  widgetShow rCount
                  widgetShow noteDurCombo
      updateNaBox :: GUICell -> IO ()
      updateNaBox GUICell { cellAction = act } = case act of
        Inert -> hideNa
        Absorb -> hideNa
        _ -> showNa

  state <- newEmptyMVar
  boardOnPress board
    (\iPos -> liftIO $ do
        postGUIAsync $ void $ tryPutMVar state iPos
        return True
    )
  boardOnRelease board
    (\fPos -> liftIO $ do
        postGUIAsync $ do
          mp <- boardGetPiece fPos board
          mstate <- tryTakeMVar state
          when (fPos `elem` validArea && isJust mp) $ do
            let piece = snd $ fromJust mp
            when (maybe False (== fPos) mstate) $
              boardSetPiece fPos (BF.second rotateGUICell (Player,piece)) board
            nmp <- boardGetPiece fPos board
            print nmp
            when (isJust nmp) $ do
              let nC = snd $ fromJust nmp
              reactiveValueWrite setRV (fPos,nC)
              fromMaybeM_ $ reactiveValueWrite artComboRV . naArt <$>
                getNAttr (cellAction nC)
              fromMaybeM_ $
                reactiveValueWrite slideComboRV . ornSlide . naOrn <$> getNAttr (cellAction nC)
              reactiveValueWrite rCountRV $ repeatCount nC
              fromMaybeM_ $ reactiveValueWrite noteDurRV . naDur <$>
                getNAttr (cellAction nC)
        return True
    )

  reactiveValueOnCanRead setRV (reactiveValueRead setRV >>=  updateNaBox . snd)

  widgetShow pieceBox
  widgetShow naBox
  return pieceBox
