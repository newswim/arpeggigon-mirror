{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}

module RMCA.GUI.MultiBoard where

import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Array
import           Data.List
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

-- In GTk, a “thing with tabs” has the, I think, very confusing name
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
                                  , ReactiveFieldWrite IO [PlayHead]
                                  ))
                     , ReactiveFieldReadWrite IO Int
                     )
createNotebook addLayerRV rmLayerRV layerMCBMVar guiCellMCBMVar = do
  n <- notebookNew
  let curPageRV = ReactiveFieldReadWrite setter getter notifier
        where (ReactiveFieldRead getter _) = notebookGetCurrentPagePassive n
              -- afterSwitchPage is deprecated but switchPage gets us
              -- the old page number and not the new one and using
              -- afterSwitchPage doesn't trigger a warning.
              setter = postGUIAsync . notebookSetCurrentPage n
              notifier io = void $ afterSwitchPage n (const io)

  pageChanRV <- newCBMVarRW []
  let foundHole = let foundHole' [] = 0
                      foundHole' (x:[]) = x + 1
                      foundHole' (x:y:xs) = if x + 1 /= y then x + 1 else foundHole (y:xs)
                  in foundHole' . sort


  let curChanRV = liftR2 (!!) pageChanRV curPageRV
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
                when (button == RightButton && maybe False (== fPos) mstate) $ do
                  let nCell = rotateGUICell piece
                  --boardSetPiece fPos nPiece ioBoard
                  reactiveValueWrite guiCellMCBMVar nCell
                nmp <- boardGetPiece fPos ioBoard
                when (button == LeftButton && isJust nmp) $ do
                  let nCell = snd $ fromJust nmp
                  mOHid <- tryTakeMVar guiCellHidMVar
                  when (isJust mOHid) $ do
                    print "Removing."
                    removeCallbackMCBMVar guiCellMCBMVar $ fromJust mOHid
                  reactiveValueWrite guiCellMCBMVar nCell
                  nHid <- installCallbackMCBMVar guiCellMCBMVar $ do
                    cp <- reactiveValueRead curChanRV
                    guiVal <- reactiveValueRead guiCellMCBMVar
                    print guiVal
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

  fstP <- notebookAppendPage n boardCont "Lol first"
  notebookPageNumber <- newCBMVarRW 1

  initBoardRV guiBoard >>=
    \(boardRV, pieceArrRV, phRV) -> reactiveValueRead chanMapRV >>=
    reactiveValueWrite chanMapRV . M.insert fstP (boardRV,pieceArrRV,phRV)

  reactiveValueRead pageChanRV >>=
    reactiveValueWrite pageChanRV . (\pc -> pc ++ [foundHole pc])
  layerMapRV <- newCBMVarRW $ M.insert fstP defaultLayer M.empty

  let updateLayer cp = do
        nLayer <- reactiveValueRead layerMCBMVar
        reactiveValueRead layerMapRV >>=
          reactiveValueWrite layerMapRV . M.insert cp nLayer

  layerHidMVar <- newEmptyMVar

  installCallbackMCBMVar layerMCBMVar
    (reactiveValueRead curChanRV >>= updateLayer) >>= putMVar layerHidMVar

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
      nCenterBoard <- alignmentNew 0.5 0.5 0 0
      containerAdd nCenterBoard nGuiBoard
      containerAdd nBoardCont nCenterBoard

      newP <- notebookAppendPage n nBoardCont $ show np
      pChan <- reactiveValueRead pageChanRV
      let newCP = foundHole pChan
      print ("newP" ++ " " ++ show newP)
      (nBoardRV, nPieceArrRV, nPhRV) <- initBoardRV nGuiBoard

      reactiveValueRead chanMapRV >>=
        reactiveValueWrite chanMapRV . M.insert newCP (nBoardRV,nPieceArrRV,nPhRV)
      reactiveValueRead layerMapRV >>=
        reactiveValueWrite layerMapRV . M.insert newCP defaultLayer

      --reactiveValueWrite curPageRV newP
      reactiveValueWrite pageChanRV (pChan ++ [newCP])
      widgetShowAll n

  reactiveValueOnCanRead rmLayerRV $ postGUIAsync $ do
    np <- reactiveValueRead notebookPageNumber
    when (np > 1) $ do
      cp <- reactiveValueRead curPageRV
      oldCP <- reactiveValueRead curChanRV
      let rmIndex :: Int -> [a] -> [a]
          rmIndex n l = take n l ++ drop (n + 1) l
      notebookRemovePage n cp

      reactiveValueRead pageChanRV >>=
        reactiveValueWrite pageChanRV . rmIndex cp

      reactiveValueRead notebookPageNumber >>=
        reactiveValueWrite notebookPageNumber . subtract 1

      reactiveValueRead chanMapRV >>=
        reactiveValueWrite chanMapRV . M.delete oldCP
      reactiveValueRead layerMapRV >>=
        reactiveValueWrite layerMapRV . M.delete oldCP

      --updateRV curPageRV

    widgetShowAll n
    return ()

  reactiveValueOnCanRead curChanRV $ do
    cp <- reactiveValueRead curChanRV
    print cp
    when (cp >= 0) $ do
      reactiveValueRead pageChanRV >>= print
      takeMVar layerHidMVar >>= removeCallbackMCBMVar layerMCBMVar
      layerMap <- reactiveValueRead layerMapRV
      --print $ M.keys layerMap
      let mSelLayer = M.lookup cp layerMap
      when (isNothing mSelLayer) $ error "Not found selected layer!"
      let selLayer = fromJust mSelLayer
      reactiveValueWrite layerMCBMVar selLayer
      installCallbackMCBMVar layerMCBMVar (updateLayer cp) >>=
        putMVar layerHidMVar
      return ()

  ------------------------------------------------------------------------------
  -- For good measure
  ------------------------------------------------------------------------------
  return (n, chanMapRV, curPageRV)
