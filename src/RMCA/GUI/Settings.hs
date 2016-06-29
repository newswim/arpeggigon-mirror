{-# LANGUAGE ScopedTypeVariables #-}

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
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.Board.TiledBoard hiding (Board)
import           Graphics.UI.Gtk.Reactive
import           RMCA.Auxiliary.RV
import           RMCA.GUI.Board
import           RMCA.Semantics

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
            when (maybe False (== fPos) mstate) $
              boardSetPiece fPos (BF.second rotateGUICell $
                                  fromJust mp) board
            let hideNa :: IO ()
                hideNa = widgetHide slideCombo >> widgetHide artCombo
                showNa :: IO ()
                showNa = widgetShow slideCombo >> widgetShow artCombo
                updateNaBox :: GUICell -> IO ()
                updateNaBox GUICell { cellAction = act } = case act of
                  Inert -> hideNa
                  Absorb -> hideNa
                  _ -> print "Show!" >> showNa
                pieceRV = pieceArrRV ! fPos
                piece = snd $ fromJust mp
            updateNaBox piece
            setRV <- newCBMVarRW $ piece
            reactiveValueOnCanRead slideComboRV $ do
              nSlide <- reactiveValueWrite slideComboRV
              oCell <- reactiveValueRead setRV
              reactiveValueWrite setRV (setSlide oCell nSlide)
            setRV =:> pieceRV
            reactiveValueOnCanRead setRV $ updateNaBox $ piece
        return True
    )
  widgetShow pieceBox >> widgetShow naBox
  return pieceBox
