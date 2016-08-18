{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}

module RMCA.GUI.MultiBoard where

import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Array
import qualified Data.IntMap                                as M
import           Data.List
import           Data.Maybe
import           Data.ReactiveValue
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.Board.TiledBoard           hiding (Board)
import           Graphics.UI.Gtk.Layout.BackgroundContainer
import           Graphics.UI.Gtk.Reactive.Gtk2
import           RMCA.Auxiliary
import           RMCA.Global.Clock
import           RMCA.GUI.Board
import           RMCA.Layer.Layer
import           RMCA.MCBMVar
import           RMCA.Semantics

createNotebook :: ( ReactiveValueRead addLayer () IO
                  , ReactiveValueRead rmLayer () IO
                  ) =>
                  TickableClock
               -> addLayer
               -> rmLayer
               -> MCBMVar Layer
               -> MCBMVar GUICell
               -> IO ( Notebook
                     , ReactiveFieldRead IO (M.IntMap Board)
                     , ReactiveFieldRead IO (M.IntMap Layer)
                     , ReactiveFieldRead IO
                       (M.IntMap (ReactiveFieldWrite IO [PlayHead]))
                     )
createNotebook tc addLayerRV rmLayerRV layerMCBMVar guiCellMCBMVar = do
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
                      foundHole' [x] = x + 1
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
          (\iPos' -> liftIO $ do
              let iPos = actualTile iPos'
              postGUIAsync $ void $ tryPutMVar state iPos
              return True
          )
        boardOnRelease ioBoard
          (\fPos' -> do
              let fPos = actualTile fPos'
              button <- eventButton
              liftIO $ postGUIAsync $ do
                mp <- boardGetPiece fPos ioBoard
                mstate <- tryTakeMVar state
                when (fPos `elem` validArea && isJust mp) $ do
                  let piece = snd $ fromJust mp
                  when (button == RightButton && maybe False (== fPos) mstate) $ do
                    let nCell = rotateGUICell piece
                    boardSetPiece fPos (Player,nCell) ioBoard
                  nmp <- boardGetPiece fPos ioBoard
                  when (button == LeftButton && isJust nmp) $ do
                    let nCell = snd $ fromJust nmp
                    mOHid <- tryTakeMVar guiCellHidMVar
                    forM_ mOHid $ removeCallbackMCBMVar guiCellMCBMVar
                    reactiveValueWrite guiCellMCBMVar nCell
                    nHid <- installCallbackMCBMVar guiCellMCBMVar $ do
                      cp <- reactiveValueRead curChanRV
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

  fstP <- notebookAppendPage n boardCont "Lol first"
  notebookPageNumber <- newCBMVarRW (1 :: Int)

  initBoardRV tc guiBoard >>=
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

      notebookAppendPage n nBoardCont $ show np
      pChan <- reactiveValueRead pageChanRV
      let newCP = foundHole pChan
      (nBoardRV, nPieceArrRV, nPhRV) <- initBoardRV tc nGuiBoard

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
    when (cp >= 0) $ do
      takeMVar layerHidMVar >>= removeCallbackMCBMVar layerMCBMVar
      layerMap <- reactiveValueRead layerMapRV
      let mSelLayer = M.lookup cp layerMap
      when (isNothing mSelLayer) $ error "Not found selected layer!"
      let selLayer = fromJust mSelLayer
      reactiveValueWrite layerMCBMVar selLayer
      installCallbackMCBMVar layerMCBMVar (updateLayer cp) >>=
        putMVar layerHidMVar
      return ()

    oldCurChanRV <- newCBMVarRW =<< reactiveValueRead curChanRV
    reactiveValueOnCanRead curChanRV $ do
      oldC <- reactiveValueRead oldCurChanRV
      newC <- reactiveValueRead curChanRV
      when (oldC /= newC) $ do
        reactiveValueWrite oldCurChanRV newC
        tryTakeMVar guiCellHidMVar >>=
          fromMaybeM_ . fmap (removeCallbackMCBMVar guiCellMCBMVar)
        reactiveValueWrite guiCellMCBMVar inertCell

  ------------------------------------------------------------------------------
  -- Flatten maps
  ------------------------------------------------------------------------------
  let {-phMapRV :: ReactiveFieldWrite IO (M.IntMap [PlayHead])
      phMapRV = ReactiveFieldWrite setter
        where setter phM = sequence_ $ M.mapWithKey writePhs phM
              writePhs :: Int -> [PlayHead] -> IO ()
              writePhs k phs = do chanMap <- reactiveValueRead chanMapRV
                                  let mselChan = M.lookup k chanMap
                                  when (isNothing mselChan) $
                                    error "Can't find layer!"
                                  let (_,_,phsRV) = fromJust mselChan
                                  reactiveValueWrite phsRV phs
-}
      phMapRV :: ReactiveFieldRead IO (M.IntMap (ReactiveFieldWrite IO [PlayHead]))
      phMapRV = liftR (M.map (\(_,_,b) -> b)) chanMapRV

      boardMapRV :: ReactiveFieldRead IO (M.IntMap Board)
      boardMapRV = ReactiveFieldRead getter notifier
        where notifier io = do
                chanMap <- reactiveValueRead chanMapRV
                mapM_ ((`reactiveValueOnCanRead` io) . \(b,_,_) -> b) chanMap
              getter = do
                chanMap <- reactiveValueRead chanMapRV
                mapM (reactiveValueRead . \(b,_,_) -> b) chanMap

  return (n, boardMapRV, readOnly layerMapRV, phMapRV)
