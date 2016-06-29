{-# LANGUAGE ScopedTypeVariables #-}

module RMCA.GUI.Settings where

import Control.Monad
import Data.Array
import Data.Maybe
import Data.ReactiveValue
import Data.String
import Data.Tuple
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Board.TiledBoard hiding (Board)
import Graphics.UI.Gtk.Reactive
import RMCA.Auxiliary.RV
import RMCA.GUI.Board
import RMCA.Semantics

comboBoxIndexRV :: (ComboBoxClass box) =>
                   box -> ReactiveFieldReadWrite IO Int
comboBoxIndexRV box = ReactiveFieldReadWrite setter getter notifier
  where getter = comboBoxGetActive box
        setter = comboBoxSetActive box
        notifier = void . on box changed

pieceButtons :: Array Pos (ReactiveFieldWrite IO GUICell)
             -> IOBoard
             -> VBox
             -> IO VBox
pieceButtons rvArray board pieceBox = do
  naBox <- vBoxNew False 10

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

  let displayPieceInfo :: (Int,Int) -> IO ()
      displayPieceInfo i = do
        print i
        when (i `elem` validArea) $ do
          let pieceRV = rvArray ! i
          piece <- boardGetPiece i board
          when (isJust piece) $ do
            setRV <- newCBMVarRW $ snd $ fromJust piece
            setRV =:> pieceRV
            reactiveValueOnCanRead setRV $ updateNaBox $ snd $ fromJust piece
      hideNa :: IO ()
      hideNa = widgetHide slideCombo >> widgetHide artCombo
      showNa :: IO ()
      showNa = widgetShow slideCombo >> widgetShow artCombo
      updateNaBox :: GUICell -> IO ()
      updateNaBox GUICell { cellAction = act } = case act of
        Inert -> hideNa
        Absorb -> hideNa
        _ -> showNa

  boardOnClick board displayPieceInfo
  boxPackStart pieceBox naBox PackNatural 10
  print "Coucou !"
  return pieceBox
