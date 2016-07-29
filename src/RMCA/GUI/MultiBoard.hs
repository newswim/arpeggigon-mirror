{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}

module RMCA.GUI.MultiBoard where

import           Control.Arrow
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Array
import qualified Data.Map                                   as M
import           Data.Maybe
import           Data.ReactiveValue
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.Board.BoardLink
import           Graphics.UI.Gtk.Board.TiledBoard           hiding (Board)
import           Graphics.UI.Gtk.Layout.BackgroundContainer
import           Graphics.UI.Gtk.Reactive.Gtk2
import           RMCA.Auxiliary
import           RMCA.GUI.Board
import           RMCA.Layer.Layer
import           RMCA.MCBMVar
import           RMCA.Semantics

-- In GTk, a “thing with tabs” has the I think very confusing name
-- Notebook.

createNotebook :: ( ReactiveValueRead addLayer () IO
                  , ReactiveValueRead rmLayer () IO
                  ) =>
                  addLayer
               -> rmLayer
               -> MCBMVar Layer
               -> MCBMVar GUICell
               -> IO ( Notebook
                     , ReactiveFieldReadWrite IO
                       (M.Map Int ( ReactiveFieldRead IO Board
                                  , Array Pos (ReactiveFieldWrite IO GUICell)
                                  , ReactiveFieldWrite IO [PlayHead])
                       )
                     , ReactiveFieldReadWrite IO Int
                     )
createNotebook addLayerRV rmLayerRV layerMCBMVar guiCellMCBMVar = do
  n <- notebookNew
  let curPageRV = ReactiveFieldReadWrite setter getter notifier
      (ReactiveFieldRead getter notifier) = notebookGetCurrentPagePassive n
      (ReactiveFieldWrite setter) = notebookSetCurrentPageReactive n
  ------------------------------------------------------------------------------
  -- First board
  ------------------------------------------------------------------------------

  chanMapRV <- newCBMVarRW M.empty
  guiCellHidMVar <- newEmptyMVar
  let clickHandler ioBoard = do
        state <- newEmptyMVar
        boardOnPress ioBoard
          (\iPos -> liftIO $ do
              postGUIAsync $ void $ tryPutMVar state iPos
              return True
          )
        boardOnRelease ioBoard
          (\fPos -> do
            button <- eventButton
            liftIO $ postGUIAsync $ do
              mp <- boardGetPiece fPos ioBoard
              mstate <- tryTakeMVar state
              when (fPos `elem` validArea && isJust mp) $ do
                let piece = snd $ fromJust mp
                when (button == RightButton && maybe False (== fPos) mstate) $
                  boardSetPiece fPos (second rotateGUICell (Player,piece)) ioBoard
                nmp <- boardGetPiece fPos ioBoard
                when (button == LeftButton && isJust nmp) $ do
                  let nCell = snd $ fromJust nmp
                  reactiveValueWrite guiCellMCBMVar nCell
                  mOHid <- tryTakeMVar guiCellHidMVar
                  when (isJust mOHid) $
                    removeCallbackMCBMVar guiCellMCBMVar $ fromJust mOHid
                  nHid <- installCallbackMCBMVar guiCellMCBMVar $ do
                    cp <- reactiveValueRead curPageRV
                    guiVal <- reactiveValueRead guiCellMCBMVar
                    mChanRV <- M.lookup cp <$> reactiveValueRead chanMapRV
                    when (isNothing mChanRV) $ error "Can't get piece array!"
                    let (_,pieceArrRV,_) = fromJust mChanRV
                    reactiveValueWrite (pieceArrRV ! fPos) guiVal
                  putMVar guiCellHidMVar nHid
            return True
          )

  boardCont <- backgroundContainerNew
  guiBoard <- attachGameRules =<< initGame
  clickHandler guiBoard
  centerBoard <- alignmentNew 0.5 0.5 0 0
  containerAdd centerBoard guiBoard
  containerAdd boardCont centerBoard

  fstP <- notebookPrependPage n boardCont "Lol first"
  notebookPageNumber <- newCBMVarRW 1

  initBoardRV guiBoard >>=
    \(boardRV, pieceArrRV, phRV) -> reactiveValueRead chanMapRV >>=
    reactiveValueWrite chanMapRV . M.insert fstP (boardRV,pieceArrRV,phRV)

  layerMapRV <- newCBMVarRW $ M.insert fstP defaultLayer M.empty

  let updateLayer cp = do
        nLayer <- reactiveValueRead layerMCBMVar
        reactiveValueRead layerMapRV >>=
          reactiveValueWrite layerMapRV . M.insert cp nLayer

  layerHidMVar <- newEmptyMVar

  installCallbackMCBMVar layerMCBMVar
    (reactiveValueRead curPageRV >>= updateLayer) >>= putMVar layerHidMVar

  ------------------------------------------------------------------------------
  -- Following boards
  ------------------------------------------------------------------------------

  reactiveValueOnCanRead addLayerRV $ postGUIAsync $ do
    np <- reactiveValueRead notebookPageNumber
    unless (np >= 16) $ do
      reactiveValueWrite notebookPageNumber (np + 1)
      nBoardCont <- backgroundContainerNew

      nGuiBoard <- attachGameRules =<< initGame
      clickHandler nGuiBoard
      centerBoard <- alignmentNew 0.5 0.5 0 0
      containerAdd centerBoard nGuiBoard
      containerAdd nBoardCont centerBoard

      newP <- notebookAppendPage n boardCont "sdlkfhd"
      (nBoardRV, nPieceArrRV, nPhRV) <- initBoardRV nGuiBoard

      reactiveValueRead chanMapRV >>=
        reactiveValueWrite chanMapRV . M.insert newP (nBoardRV,nPieceArrRV,nPhRV)

      reactiveValueWrite curPageRV newP

      widgetShowAll n

  reactiveValueOnCanRead rmLayerRV $ postGUIAsync $ do
    np <- reactiveValueRead notebookPageNumber
    when (np > 1) $ do
      cp <- notebookGetCurrentPage n
      notebookRemovePage n cp

      reactiveValueRead notebookPageNumber >>=
        reactiveValueWrite notebookPageNumber . subtract 1

      reactiveValueRead chanMapRV >>=
        reactiveValueWrite chanMapRV . M.delete cp
      reactiveValueRead layerMapRV >>=
        reactiveValueWrite layerMapRV . M.delete cp

    widgetShowAll n
    return ()

  reactiveValueOnCanRead curPageRV $ do
    takeMVar layerHidMVar >>= removeCallbackMCBMVar layerMCBMVar
    cp <- reactiveValueRead curPageRV
    layerMap <- reactiveValueRead layerMapRV
    let mSelLayer = M.lookup cp layerMap
    when (isNothing mSelLayer) $ error "Not found selected layer!"
    let selLayer = fromJust mSelLayer
    reactiveValueWrite layerMCBMVar selLayer
    installCallbackMCBMVar layerMCBMVar (updateLayer cp) >>= putMVar layerHidMVar
    return ()

  ------------------------------------------------------------------------------
  -- Handle clicks
  ------------------------------------------------------------------------------





  ------------------------------------------------------------------------------
  -- For good measure
  ------------------------------------------------------------------------------
  return (n, chanMapRV, curPageRV)
  --return ()
