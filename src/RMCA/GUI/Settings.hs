{-# LANGUAGE ScopedTypeVariables, TupleSections #-}

module RMCA.GUI.Settings where

import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Array
import qualified Data.Bifunctor                   as BF
import           Data.Maybe
import           Data.ReactiveValue
import           Data.String
import           Data.Tuple
import           Graphics.UI.Gtk                  hiding (Action)
import           Graphics.UI.Gtk.Board.TiledBoard hiding (Board)
import           RMCA.Auxiliary.RV
import           RMCA.GUI.Board
import           RMCA.Semantics

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
      artComboRV = liftRW (bijection (indexToArt,artToIndex)) $
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
      slideComboRV = liftRW (bijection (indexToSlide,slideToIndex)) $
                     comboBoxIndexRV slideCombo


  state <- newEmptyMVar

  -- Side RV
  setRV <- newCBMVarRW ((0,0),inertCell)

  reactiveValueOnCanRead slideComboRV $ do
    nSlide <- reactiveValueRead slideComboRV
    (i,oCell) <- reactiveValueRead setRV
    let nCa :: Maybe NoteAttr
        nCa = (\na -> na { naOrn = (naOrn na) { ornSlide = nSlide } }) <$>
              (getNAttr $ cellAction oCell)
        nCell :: GUICell
        nCell = if (isJust nCa)
                then oCell { cellAction =
                             setNAttr (fromJust nCa) (cellAction oCell)
                           }
                else oCell
    reactiveValueWrite setRV (i,nCell)

  reactiveValueOnCanRead artComboRV $ do
    nArt <- reactiveValueRead artComboRV
    (i,oCell) <- reactiveValueRead setRV
    let nCa :: Maybe NoteAttr
        nCa = getNAttr $ cellAction oCell
        nCell :: GUICell
        nCell = if (isJust nCa)
                then oCell { cellAction =
                             setNAttr (fromJust nCa) (cellAction oCell) }
                else oCell
    reactiveValueWrite setRV (i,nCell)

  let hideNa :: IO ()
      hideNa = widgetHide slideCombo >> widgetHide artCombo
      showNa :: IO ()
      showNa = widgetShow slideCombo >> widgetShow artCombo
      updateNaBox :: GUICell -> IO ()
      updateNaBox GUICell { cellAction = act } = case act of
        Inert -> hideNa
        Absorb -> hideNa
        _ -> showNa

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
            when (maybe False (== fPos) mstate) $ do
              boardSetPiece fPos (BF.second rotateGUICell (Player,piece)) board
            nmp <- boardGetPiece fPos board
            when (isJust nmp) $ reactiveValueWrite setRV $ (fPos,snd $ fromJust nmp)
        return True
    )

  reactiveValueOnCanRead setRV $ do
    (i,c) <- reactiveValueRead setRV
    reactiveValueWrite (pieceArrRV ! i) c
    updateNaBox c
  widgetShow pieceBox >> widgetShow naBox
  return pieceBox
