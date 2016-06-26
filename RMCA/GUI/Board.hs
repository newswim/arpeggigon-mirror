{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables,
             TypeSynonymInstances #-}

module RMCA.GUI.Board where

import           Control.Monad
import           Data.Array
import           Data.Array.MArray
import qualified Data.Bifunctor                   as BF
import           Data.Board.GameBoardIO
import           Data.CBMVar
import           Data.Maybe
import           Data.Ratio
import           Data.ReactiveValue
import           Debug.Trace
import           Game.Board.BasicTurnGame
import           Graphics.UI.Gtk                  hiding (Action)
import           Graphics.UI.Gtk.Board.BoardLink
import           Graphics.UI.Gtk.Board.TiledBoard hiding (Board)
import qualified Graphics.UI.Gtk.Board.TiledBoard as BIO
import           RMCA.Semantics

data GUICell = GUICell { cellAction  :: Action
                       , repeatCount :: Int
                       , asPh        :: Bool
                       }

newtype GUIBoard = GUIBoard { toGS :: GameState Int Tile Player GUICell }

data Tile = Tile
data Player = Player deriving(Show)

-- Takes a GUI coordinate and give the corresponding coordinate on the
-- internal board
fromGUICoords :: (Int,Int) -> (Int,Int)
fromGUICoords (x,y) = (x,(x `mod` 2 - y) `quot` 2)

-- Takes coordinates from the point of view of the internal board and
-- translates them to GUI board coordinates.
toGUICoords :: (Int,Int) -> (Int,Int)
toGUICoords (x,y) = (x,2*(-y) + x `mod` 2)

tileW :: Int
tileW = 40

tileH :: Int
tileH = round (sqrt 3 * fromIntegral tileW / 3)

hexW :: Int
hexW = round (4 * fromIntegral tileW / 3)

hexH :: Int
hexH = round (sqrt 3 * fromIntegral hexW / 2)

xMax, yMax :: Int
(xMax,yMax) = BF.second (*2) $ neighbor N nec
xMin, yMin :: Int
(xMin,yMin) = BF.second (*2) swc

boardToTile :: [(Int,Int,Tile)]
boardToTile = [(x,y,Tile) | (x,y) <- range ( (xMin-1,yMin)
                                           , (xMax+1,yMax+1))]



boardToPiece :: [PlayHead] -> Board -> [(Int,Int,Player,GUICell)]
boardToPiece ph = map placePiece . filter (onBoard . fst) . assocs
  where placePiece :: (Pos,Cell) -> (Int,Int,Player,GUICell)
        placePiece ((x,y),(a,n)) = let y' = 2*(-y) + x `mod` 2
                                       c = GUICell { cellAction = a
                                                   , repeatCount = n
                                                   , asPh = (x,y) `elem` phPosS
                                                   }
                                   in (x,y',Player,c)
        phPosS = map phPos ph

validArea :: Board -> [(Int,Int)]
validArea = map (\(x,y,_,_) -> (x,y)) . boardToPiece []

na = NoteAttr {
          naArt = Accent13,
          naDur = 1 % 1,
          naOrn = Ornaments Nothing [] NoSlide
      }

initGUIBoard :: GUIBoard
initGUIBoard = GUIBoard GameState
  { curPlayer'   = Player
  , boardPos     = boardToTile
  , boardPieces' = boardToPiece [] $
                   makeBoard [((0,5), mkCell (ChDir True na NE))]
  }

instance PlayableGame GUIBoard Int Tile Player GUICell where
  curPlayer _               = Player
  allPos (GUIBoard game)    = boardPos game
  allPieces (GUIBoard game) = boardPieces' game
  moveEnabled _             = True
  canMove (GUIBoard game) _ (x,y)
    | Just (_,p) <- getPieceAt game (x,y)
    , GUICell { cellAction = Inert } <- p = False
    | otherwise = True
  canMoveTo _ _ _ (x,y) = (x,y) `elem` validArea
    where validArea = map (\(x',y',_,_) -> (x',y')) $ boardToPiece [] $
                      makeBoard []

  move (GUIBoard game) _ iPos@(_,yi) (xf,yf) = [ MovePiece iPos fPos'
                                               , AddPiece iPos Player nCell]
    where fPos'
            |    (xf `mod` 2 == 0 && yf `mod` 2 == 0)
              || (xf `mod` 2 /= 0 && yf `mod` 2 /= 0) = (xf,yf)
            | otherwise = (xf,yf+signum' (yf-yi))
          signum' x
            | x == 0 = 1
            | otherwise = signum x
          nCell
            | Just (_,GUICell { asPh = ph, repeatCount = n }) <-
                getPieceAt game iPos = inertCell { repeatCount = n
                                                 , asPh = ph
                                                 }
            | otherwise = inertCell
            where inertCell = GUICell { cellAction = Inert
                                      , repeatCount = 1
                                      , asPh = False}

  applyChange (GUIBoard game) (AddPiece pos@(x,y) Player piece) =
    GUIBoard $ game { boardPieces' = bp' }
    where bp' = (x,y,Player,piece):boardPieces' game

  applyChange (GUIBoard game) (RemovePiece (x,y)) = GUIBoard $
    game { boardPieces' = bp' }
    where bp' = [p | p@(x',y',_,_) <- boardPieces' game
                   , x /= x' || y /= y']

  applyChange guiBoard@(GUIBoard game) (MovePiece iPos fPos)
    | Just (_,p) <- getPieceAt game iPos
    = applyChanges guiBoard [ RemovePiece iPos
                            , RemovePiece fPos
                            , AddPiece fPos Player p]
    | otherwise = guiBoard

initGame :: IO (Game GUIBoard Int Tile Player GUICell)
initGame = do
  pixbufs <- fileToPixbuf
  tilePixbuf <- pixbufNew ColorspaceRgb False 8 tileW tileH
  pixbufFill tilePixbuf 50 50 50 0
  let pixPiece :: (Player,GUICell) -> Pixbuf
      pixPiece (_,a) = fromJust $ lookup (actionToFile a) pixbufs
      pixTile :: Tile -> Pixbuf
      pixTile _ = tilePixbuf
      visualA = VisualGameAspects { tileF = pixTile
                                  , pieceF = pixPiece
                                  , bgColor = (1000,1000,1000)
                                  , bg = Nothing
                                  }

  return $ Game visualA initGUIBoard

initBoardRV :: BIO.Board Int Tile (Player,GUICell)
            -> IO ( ReactiveFieldRead IO Board
                  , ReactiveFieldReadWrite IO [PlayHead])
initBoardRV BIO.Board { boardPieces = GameBoard array } = do
  phMVar <- newCBMVar []
  let getterB :: IO Board
      getterB = do
        (boardArray :: [((Int,Int),Maybe (Player,GUICell))]) <- getAssocs array
        let board = makeBoard $
              map (BF.first fromGUICoords .
                   BF.second ((\(_,c) -> (cellAction c,repeatCount c)) .
                              fromJust)) $
              filter (isJust . snd) boardArray
        return board

      notifierB :: IO () -> IO ()
      notifierB _ = return ()

      getterP :: IO [PlayHead]
      getterP = readCBMVar phMVar

      setterP :: [PlayHead] -> IO ()
      setterP lph = do
        writeCBMVar phMVar lph
        boardArray <- getAssocs array
        let phPosS = map (toGUICoords . phPos) lph
            updatePh :: ((Int,Int),Maybe (Player,GUICell)) -> IO ()
            updatePh (i,c) = when (isJust c) $ do
              let (_,c') = fromJust c
              writeArray array i (Just (Player,c' { asPh = i `elem` phPosS }))
        mapM_ updatePh boardArray

      notifierP :: IO () -> IO ()
      notifierP = installCallbackCBMVar phMVar

      b = ReactiveFieldRead getterB notifierB
      ph = ReactiveFieldReadWrite setterP getterP notifierP
  return (b,ph)

fileToPixbuf :: IO [(FilePath,Pixbuf)]
fileToPixbuf = mapM (\f -> let f' = "img/" ++ f in uncurry (liftM2 (,))
                      ( return f'
                      , pixbufNewFromFile f' >>=
                        \p -> pixbufScaleSimple p hexW hexW InterpBilinear ))
               (["hexOn.png","stop.svg","split.svg"] ++
                concat [["start" ++ show d ++ ".svg","ric" ++ show d ++ ".svg"]
                       | d <- [N .. NW]])

actionToFile :: GUICell -> FilePath
actionToFile GUICell { cellAction = a
                     , asPh = bool
                     } =
  case (a,bool) of
    (Inert,True) -> "img/hexOn.png"
    (Inert,False) -> "img/hexOff.png"
    (Absorb,_) -> "img/stop.svg"
    (Stop _,_) -> "img/stop.svg"
    (ChDir True _ d,_) -> "img/start" ++ show d ++ ".svg"
    (ChDir False _ d,_) -> "img/ric" ++ show d ++ ".svg"
    (Split _,_) -> "img/split.svg"
