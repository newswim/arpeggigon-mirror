{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, TupleSections #-}

module RMCA.GUI.MultiBoard where

import Debug.Trace
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Array
import           Data.CBRef
import qualified Data.IntMap                                as M
import           Data.List
import           Data.Maybe
import           Data.ReactiveValue
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.Board.TiledBoard           hiding (Board)
import           Graphics.UI.Gtk.Layout.BackgroundContainer
import           Graphics.UI.Gtk.Reactive.Gtk2
import           RMCA.Auxiliary
import           RMCA.GUI.Board
import           RMCA.IOClockworks
import           RMCA.Layer.LayerConf
import           RMCA.MCBMVar
import           RMCA.ReactiveValueAtomicUpdate
import           RMCA.Semantics
import           RMCA.Translator.Message

maxLayers :: Int
maxLayers = 16

layerName :: String
layerName = "Layer"

createNotebook :: ( ReactiveValueRead addLayer () IO
                  , ReactiveValueRead rmLayer () IO
                  , ReactiveValueAtomicUpdate board (M.IntMap ([Note],[Message])) IO
                  ) =>
                  board
               -> IOTick
               -> addLayer
               -> rmLayer
               -> MCBMVar StaticLayerConf
               -> MCBMVar DynLayerConf
               -> MCBMVar SynthConf
               -> MCBMVar GUICell
               -> IO ( Notebook
                     , ReactiveFieldRead IO (M.IntMap Board)
                     , CBRef (M.IntMap LayerConf)
                     , ReactiveFieldRead IO
                       (M.IntMap (ReactiveFieldWrite IO [PlayHead]))
                     )
createNotebook boardQueue tc addLayerRV rmLayerRV
  statMCBMVar dynMCBMVar synthMCBMVar guiCellMCBMVar = do
  n <- notebookNew
  let curPageRV = ReactiveFieldReadWrite setter getter notifier
        where (ReactiveFieldRead getter _) = notebookGetCurrentPagePassive n
              -- afterSwitchPage is deprecated but switchPage gets us
              -- the old page number and not the new one and using
              -- afterSwitchPage doesn't trigger a warning so…
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
                    maybe (return ()) (removeCallbackMCBMVar guiCellMCBMVar) mOHid
                    reactiveValueWrite guiCellMCBMVar nCell
                    nHid <- installCallbackMCBMVar guiCellMCBMVar $ do
                      cp <- reactiveValueRead curChanRV
                      guiVal <- reactiveValueRead guiCellMCBMVar
                      mChanRV <- fmap (M.lookup cp)
                                      (reactiveValueRead chanMapRV)
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

  fstP <- notebookAppendPage n boardCont layerName
  notebookPageNumber <- newCBMVarRW (1 :: Int)

  initBoardRV tc guiBoard >>=
    \(boardRV, pieceArrRV, phRV) -> reactiveValueRead chanMapRV >>=
    reactiveValueWrite chanMapRV . M.insert fstP (boardRV,pieceArrRV,phRV)

  reactiveValueRead pageChanRV >>=
    reactiveValueWrite pageChanRV . (\pc -> pc ++ [foundHole pc])

  layerMapRV <- newCBRef $ M.insert fstP defaultLayerConf M.empty
  reactiveValueOnCanRead layerMapRV $ do
    synth <- fmap (fmap (\(_,_,s) -> s)) (reactiveValueRead layerMapRV)
    sequence_ $ M.elems $ M.mapWithKey
      (\chan mess -> reactiveValueAppend boardQueue $
        M.singleton chan $ ([],) $ synthMessage chan mess) synth

  let updateDynLayer cp = do
        nDyn <- reactiveValueRead dynMCBMVar
        reactiveValueUpdate_ layerMapRV
          (M.adjust (\(stat,_,synth) -> (stat,nDyn,synth)) cp)
      updateSynth cp = do
        nSynth <- reactiveValueRead synthMCBMVar
        reactiveValueUpdate_ layerMapRV
          (M.adjust (\(stat,dyn,_) -> (stat,dyn,nSynth)) cp)
        reactiveValueAppend boardQueue $
          M.singleton cp $ ([],) $ synthMessage cp nSynth
      updateStatLayer cp = do
        nStat <- reactiveValueRead statMCBMVar
        reactiveValueUpdate_ layerMapRV
          (M.adjust (\(_,dyn,synth) -> (nStat,dyn,synth)) cp)

  statHidMVar <- newEmptyMVar
  dynHidMVar <- newEmptyMVar
  synthHidMVar <- newEmptyMVar

  installCallbackMCBMVar statMCBMVar
    (reactiveValueRead curChanRV >>= updateStatLayer) >>= putMVar statHidMVar
  installCallbackMCBMVar dynMCBMVar
    (reactiveValueRead curChanRV >>= updateDynLayer) >>= putMVar dynHidMVar
  installCallbackMCBMVar synthMCBMVar
    (reactiveValueRead curChanRV >>= updateSynth) >>= putMVar synthHidMVar

  ------------------------------------------------------------------------------
  -- Following boards
  ------------------------------------------------------------------------------

  reactiveValueOnCanRead addLayerRV $ postGUIAsync $ do
    np <- reactiveValueRead notebookPageNumber
    unless (np >= maxLayers) $ do
      reactiveValueWrite notebookPageNumber (np + 1)
      nBoardCont <- backgroundContainerNew

      nGuiBoard <- attachGameRules =<< initGame
      clickHandler nGuiBoard
      nCenterBoard <- alignmentNew 0.5 0.5 0 0
      containerAdd nCenterBoard nGuiBoard
      containerAdd nBoardCont nCenterBoard

      notebookAppendPage n nBoardCont layerName
      pChan <- reactiveValueRead pageChanRV
      let newCP = foundHole pChan
      (nBoardRV, nPieceArrRV, nPhRV) <- initBoardRV tc nGuiBoard

      reactiveValueRead chanMapRV >>=
        reactiveValueWrite chanMapRV . M.insert newCP (nBoardRV,nPieceArrRV,nPhRV)
      reactiveValueRead layerMapRV >>=
        reactiveValueWrite layerMapRV . M.insert newCP defaultLayerConf

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

    widgetShowAll n
    return ()

  reactiveValueOnCanRead curChanRV $ do
    cp <- reactiveValueRead curChanRV
    when (cp >= 0) $ do
      takeMVar dynHidMVar >>= removeCallbackMCBMVar dynMCBMVar
      takeMVar statHidMVar >>= removeCallbackMCBMVar statMCBMVar
      takeMVar synthHidMVar >>= removeCallbackMCBMVar synthMCBMVar
      layerMap <- reactiveValueRead layerMapRV
      let mSelLayer = M.lookup cp layerMap
      when (isNothing mSelLayer) $ error "Not found selected layer!"
      let selLayer = fromJust mSelLayer
      reactiveValueWrite dynMCBMVar (dynConf selLayer)
      installCallbackMCBMVar dynMCBMVar (updateDynLayer cp) >>=
        putMVar dynHidMVar
      reactiveValueWrite statMCBMVar (staticConf selLayer)
      installCallbackMCBMVar statMCBMVar (updateStatLayer cp) >>=
        putMVar statHidMVar
      reactiveValueWrite synthMCBMVar (synthConf selLayer)
      installCallbackMCBMVar synthMCBMVar (updateSynth cp) >>=
        putMVar synthHidMVar
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
  let phMapRV :: ReactiveFieldRead IO (M.IntMap (ReactiveFieldWrite IO [PlayHead]))
      phMapRV = liftR (M.map (\(_,_,b) -> b)) chanMapRV

      boardMapRV :: ReactiveFieldRead IO (M.IntMap Board)
      boardMapRV = ReactiveFieldRead getter notifier
        where notifier io = do
                chanMap <- reactiveValueRead chanMapRV
                intMapMapM_ ((`reactiveValueOnCanRead` io) . \(b,_,_) -> b) chanMap
              getter = do
                chanMap <- reactiveValueRead chanMapRV
                intMapMapM (reactiveValueRead . \(b,_,_) -> b) chanMap

  return (n, boardMapRV, layerMapRV, phMapRV)

------------------------------------------------------------------------------
-- IntMap versions of mapM etc. to make code work with GHC 7.8.3
------------------------------------------------------------------------------

intMapMapM_ :: (Functor m, Monad m) => (a -> m b) -> M.IntMap a -> m ()
intMapMapM_ f im = mapM_ f (M.elems im)

intMapMapM :: (Functor m, Monad m) => (a -> m b) -> M.IntMap a -> m (M.IntMap b)
intMapMapM f im = fmap (M.fromList . zip ks) (mapM f es)
    where
        (ks, es) = unzip (M.toList im)
