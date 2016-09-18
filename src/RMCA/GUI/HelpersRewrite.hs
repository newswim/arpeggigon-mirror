{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}

-- This module contains function that allow the particular geometry of
-- the board to not cause too much problems.
--
-- They are stolen from the gtk-helpers library.

module RMCA.GUI.HelpersRewrite where

import           Control.Arrow
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Array
import           Data.IORef
import           Data.Maybe
import           Data.Ratio
import           Game.Board.BasicTurnGame
import           Graphics.UI.Gtk                  hiding (Action)
import           Graphics.UI.Gtk.Board.BoardLink  hiding (attachGameRules)
import           Graphics.UI.Gtk.Board.TiledBoard hiding
    ( Board
    , boardOnPieceDragDrop
    , boardOnPieceDragOver
    , boardOnPieceDragStart
    )
import qualified Graphics.UI.Gtk.Board.TiledBoard as BIO
import           RMCA.Semantics

data GUICell = GUICell { cellAction  :: Action
                       , repeatCount :: Int
                       , asPh        :: Bool
                       } deriving(Show,Eq)

data Player = Player deriving(Show)

-- Takes a GUI coordinate and give the corresponding coordinate on the
-- internal board
fromGUICoords :: (Int,Int) -> (Int,Int)
fromGUICoords (x,y) = (x,(x `mod` 2 - y) `quot` 2)

-- Takes coordinates from the point of view of the internal board and
-- translates them to GUI board coordinates.
toGUICoords :: (Int,Int) -> (Int,Int)
toGUICoords (x,y) = (x,2*(-y) + x `mod` 2)

defNa :: NoteAttr
defNa = NoteAttr { naArt = NoAccent
                 , naDur = 1 % 4
                 , naOrn = noOrn
                 }

xMax, yMax :: Int
(xMax,yMax) = second (*2) $ neighbor N nec
xMin, yMin :: Int
(xMin,yMin) = second (*2) swc

ctrlPieces :: [(Int,Int,Player,GUICell)]
ctrlPieces = [(xMax+2,y,Player,GUICell { cellAction = action
                                       , repeatCount = 1
                                       , asPh = False
                                       })
             | let actions = [ Absorb, Stop defNa
                             , ChDir False defNa N, ChDir True defNa N
                             , Split defNa]
                     -- /!\ It would be nice to find a general formula
                     -- for placing the control pieces.
                     , (y,action) <- zip [ yMin+4,yMin+8..] actions]

ctrlCoords :: [(Int,Int)]
ctrlCoords = map (\(x,y,_,_) -> (x,y)) ctrlPieces

boardToPiece :: [PlayHead] -> Board -> [(Int,Int,Player,GUICell)]
boardToPiece ph = (++ ctrlPieces) . map placePiece .
                  filter (onBoard . fst) . assocs
  where placePiece :: (Pos,Cell) -> (Int,Int,Player,GUICell)
        placePiece ((x,y),(a,n)) = let c = GUICell { cellAction = a
                                                   , repeatCount = n
                                                   , asPh = (x,y) `elem` phPosS
                                                   }
                                       (x',y') = toGUICoords (x,y)
                                   in (x',y',Player,c)
        phPosS = map phPos ph

validArea :: [(Int,Int)]
validArea = filter (onBoard . fromGUICoords) $
            map (\(x,y,_,_) -> (x,y)) $ boardToPiece [] $ makeBoard []

-- Because of the geometry of the board, a tile might be covered by a
-- piece without actually carrying any. This function retrieves the
-- index of the tile carrying the piece that covers the tile.
actualTile :: (Int,Int) -> (Int,Int)
actualTile p@(x,y)
  | y <= yMin || p `elem` piecesCoords = p
  | otherwise = (x,y-1)
  where piecesCoords = validArea ++ ctrlCoords

boardOnPieceDragStart :: BIO.Board Int tile piece
                      -> ((Int, Int) -> IO Bool) -> IO()
boardOnPieceDragStart board f = boardOnPress board $ \ix -> do
  (x,y) <- eventCoordinates
  returning False $ liftIO $ do
    drag <- readIORef (dragEnabled board)
    when drag $ do
      canDragThis <- f ix
      let from = if canDragThis
                 then Just $ actualTile ix
                 else Nothing
          orig = if canDragThis
                 then Just (relativePos board (actualTile ix) (round x, round y))
                 else Nothing
      writeIORef (draggingFrom board) from
      writeIORef (draggingMouseOrig board) orig
      boardInvalidate board

boardOnPieceDragOver :: Ix index =>
                        BIO.Board index tile piece
                     -> ((index, index) -> (index, index) -> IO Bool) -> IO()
boardOnPieceDragOver board f = boardOnMotion board $ \ix -> do
  (x,y) <- eventCoordinates
  returning False $ liftIO $ do
    drag  <- readIORef (dragEnabled board)
    origM <- readIORef (draggingFrom board)
    when (drag && isJust origM) $ do
      canDropHere <- f (fromJust origM) ix
      let newDest = if canDropHere then Just ix else Nothing
      writeIORef (draggingTo board) newDest
      writeIORef (draggingMousePos board) (Just (round x, round y))
    boardInvalidate board

boardOnPieceDragDrop :: Ix index =>
                        BIO.Board index tile piece
                     -> ((index, index) -> (index, index) -> IO ()) -> IO()
boardOnPieceDragDrop board f = void $ do
  widgetAddEvents (boardDrawingArea board) [ButtonPressMask, ButtonReleaseMask]
  boardDrawingArea board `on` buttonReleaseEvent $ returning False $ liftIO $ do
    drag  <- readIORef (dragEnabled board)
    origM <- readIORef (draggingFrom board)
    destM <- readIORef (draggingTo board)
    let notSame = origM /= destM
    when (drag && isJust origM) $ do

      -- No longer dragging
      writeIORef (draggingFrom board)      Nothing
      writeIORef (draggingTo board)        Nothing
      writeIORef (draggingMouseOrig board) Nothing
      writeIORef (draggingMousePos board)  Nothing

      -- When possible, call the handler
      when (isJust destM && notSame) $ f (fromJust origM) (fromJust destM)

      -- In any case, the board must be repainted
      boardInvalidate board

-- This is a function shamelessy stolen and rewritten from gtk-helpers
-- to allow for hexagonal boards.
attachGameRules :: (PlayableGame a Int tile player piece) =>
                   Game a Int tile player piece
                -> IO (BIO.Board Int tile (player, piece))
attachGameRules game = do
  board <- boardNew (allPos $ gameS game) (tileF $ visual game)
           (pieceF $ visual game)

  let (r,g,b) = bgColor (visual game)
      (r', g', b') = (fromIntegral r, fromIntegral g, fromIntegral b)
  mapM_ (\s -> widgetModifyBg board s (Color r' g' b'))
    [StateNormal, StateActive, StatePrelight, StateSelected]
  when (isJust (bg $ visual game)) $
    boardSetBackground board (bg $ visual game)

  vgRef <- newIORef game

  -- Set the initial board state
  mapM_ (\(x,y) -> boardSetPiece x y board)
    [((x,y),(pl,pc)) | (x,y,pl,pc) <- allPieces (gameS game)]

  board `boardOnPieceDragStart` \pos' -> do
    let pos = actualTile pos'
    visualGame <- readIORef vgRef
    let game' = gameS visualGame
    return (moveEnabled game' && canMove game' (curPlayer game') pos)

  board `boardOnPieceDragOver` \posF' posT' -> do
    let posF = actualTile posF'
        posT = actualTile posT'
    visualGame <- readIORef vgRef
    let game' = gameS visualGame
    return (moveEnabled game' && canMoveTo game' (curPlayer game') posF posT)

  board `boardOnPieceDragDrop` \posF' posT' -> do
    let posF = actualTile posF'
        posT = actualTile posT'
    visualGame <- readIORef vgRef
    let game'  = gameS visualGame
        moves  = move game' (curPlayer game') posF posT
        game'' = foldl applyChange game' moves
    writeIORef vgRef (visualGame { gameS = game'' })
    forM_ moves (applyBoardChange board)

  when (moveEnabled (gameS game)) $ boardEnableDrag board

  return board
