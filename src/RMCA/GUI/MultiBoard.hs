{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}

module RMCA.GUI.MultiBoard where

import Control.Monad
import Data.Array
import Data.ReactiveValue
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Board.BoardLink
import Graphics.UI.Gtk.Layout.BackgroundContainer
import RMCA.Auxiliary
import RMCA.GUI.Board
import RMCA.GUI.NoteSettings
import RMCA.Layer.Layer
import RMCA.Semantics

-- In GTk, a “thing with tabs” has the I think very confusing name
-- Notebook.

createNotebook :: ( ReactiveValueRead addLayer () IO
                  , ReactiveValueRead rmLayer () IO
                  , ReactiveValueRead layer Layer IO
                  , ReactiveValueRead tempo Tempo IO
                  ) => addLayer -> rmLayer -> layer -> tempo
               -> IO ( Notebook
                     , VBox
                     , ReactiveFieldRead IO Board
                     , Array Pos (ReactiveFieldWrite IO GUICell)
                     , ReactiveFieldWrite IO [PlayHead]
                     )
createNotebook addLayerRV rmLayerRV layerRV tempoRV = do
  n <- notebookNew
  --plusImg <- imageNewFromStock gtkMediaAdd IconSizeButton
  --notebookAppendPageMenu n undefined plusImg undefined
  ------------------------------------------------------------------------------
  -- First board
  ------------------------------------------------------------------------------
  boardCont <- backgroundContainerNew
  guiBoard <- attachGameRules =<< initGame
  centerBoard <- alignmentNew 0.5 0.5 0 0
  containerAdd centerBoard guiBoard
  containerAdd boardCont centerBoard

  notebookPrependPage n boardCont "Lol first"
  notebookPageNumber <- newCBMVarRW 1

  layer <- reactiveValueRead layerRV
  tempo <- reactiveValueRead tempoRV
  (boardRV, pieceArrRV, phRV) <- initBoardRV guiBoard

  pieceBox <- clickHandling pieceArrRV guiBoard =<< vBoxNew False 10

  ------------------------------------------------------------------------------
  -- Following boards
  ------------------------------------------------------------------------------

  reactiveValueOnCanRead addLayerRV $ postGUIAsync $ do
    reactiveValueRead notebookPageNumber
      >>= reactiveValueWrite notebookPageNumber . (+1)
    boardCont <- backgroundContainerNew

    guiBoard <- attachGameRules =<< initGame
    centerBoard <- alignmentNew 0.5 0.5 0 0
    containerAdd centerBoard guiBoard
    containerAdd boardCont centerBoard

    notebookAppendPage n boardCont "sdlkfhd" >> widgetShowAll n

  reactiveValueOnCanRead rmLayerRV $ postGUIAsync $ do
    np <- reactiveValueRead notebookPageNumber
    when (np > 1) $ do
      notebookRemovePage n =<< notebookGetCurrentPage n

      reactiveValueRead notebookPageNumber
        >>= reactiveValueWrite notebookPageNumber . (subtract 1)

    widgetShowAll n
    return ()


  ------------------------------------------------------------------------------
  -- For good measure
  ------------------------------------------------------------------------------
  return (n, pieceBox, boardRV, pieceArrRV, phRV)
