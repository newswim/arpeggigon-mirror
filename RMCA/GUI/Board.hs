{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}

module RMCA.GUI.Board where

import           Control.Monad
import           Data.Array
import qualified Data.Bifunctor                  as BF
import           Data.Maybe
import           Data.Ratio
import           Game.Board.BasicTurnGame
import           Graphics.UI.Gtk                 hiding (Action)
import           Graphics.UI.Gtk.Board.BoardLink
import           RMCA.Semantics

import           Debug.Trace

newtype GUIBoard = GUIBoard { toGS :: (GameState Int Tile Player Action) }

data Tile = Tile
data Player = Player deriving(Show)

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

boardToPiece :: Board -> [(Int,Int,Player,Action)]
boardToPiece = map placePiece . filter (onBoard . fst) . assocs
  where placePiece :: (Pos,Cell) -> (Int,Int,Player,Action)
        placePiece ((x,y),(a,_)) = let y' = 2*(-y) + x `mod` 2 in
                                     (x,y',Player,a)

na = NoteAttr {
          naArt = Accent13,
          naDur = 1 % 1,
          naOrn = Ornaments Nothing [] NoSlide
      }

initGUIBoard :: GUIBoard
initGUIBoard = GUIBoard $ GameState
  { curPlayer'   = Player
  , boardPos     = boardToTile
  , boardPieces' = boardToPiece $ makeBoard [((0,5), mkCell (ChDir True na NE))]
  }


instance PlayableGame GUIBoard Int Tile Player Action where
  curPlayer _               = Player
  allPos (GUIBoard game)    = boardPos game
  allPieces (GUIBoard game) = boardPieces' game
  moveEnabled _             = True
  canMove (GUIBoard game) _ (x,y)
    | Just (_,p) <- getPieceAt game (x,y)
    , Inert <- p = False
    | otherwise = True
  canMoveTo _ _ _ (x,y) = (x,y) `elem` validArea
    where validArea = map (\(x',y',_,_) -> (x',y')) $ boardToPiece $ makeBoard []

  move _ _ iPos@(_,yi) (xf,yf) = [ MovePiece iPos fPos'
                                 , AddPiece iPos Player Inert]
    where fPos'
            |    (xf `mod` 2 == 0 && yf `mod` 2 == 0)
              || (xf `mod` 2 /= 0 && yf `mod` 2 /= 0) = (xf,yf)
            | otherwise = (xf,yf+signum' (yf-yi))
          signum' x
            | x == 0 = 1
            | otherwise = signum x


  applyChange (GUIBoard game) (AddPiece pos@(x,y) Player piece) =
    GUIBoard $ game { boardPieces' = bp' }
    where bp' = (x,y,Player,piece):(boardPieces' game)

  applyChange (GUIBoard game) (RemovePiece (x,y)) = GUIBoard $
    game { boardPieces' = bp' }
    where bp' = [p | p@(x',y',_,_) <- boardPieces' game
                   , (x /= x' || y /= y')]

  applyChange guiBoard@(GUIBoard game) (MovePiece iPos fPos)
    | Just (_,p) <- getPieceAt game iPos
    = applyChanges guiBoard [ RemovePiece iPos
                            , RemovePiece fPos
                            , AddPiece fPos Player p]
    | otherwise = guiBoard

initGame :: IO (Game GUIBoard Int Tile Player Action)
initGame = do
  pixbufs <- fileToPixbuf
  tilePixbuf <- pixbufNew ColorspaceRgb False 8 tileW tileH
  pixbufFill tilePixbuf 50 50 50 0
  let pixPiece :: (Player,Action) -> Pixbuf
      pixPiece (_,a) = fromJust $ lookup (actionToFile a) pixbufs
      pixTile :: Tile -> Pixbuf
      pixTile _ = tilePixbuf
      visualA = VisualGameAspects { tileF = pixTile
                                  , pieceF = pixPiece
                                  , bgColor = (1000,1000,1000)
                                  , bg = Nothing
                                  }

  return $ Game visualA initGUIBoard

fileToPixbuf :: IO [(FilePath,Pixbuf)]
fileToPixbuf = sequence $
  map (\f -> let f' = "img/" ++ f in uncurry (liftM2 (,))
             ( return f'
             , pixbufNewFromFile f' >>=
               \p -> pixbufScaleSimple p hexW hexW InterpBilinear )) $
  ["hexOn.png","stop.svg","split.svg"] ++
  concat [["start" ++ show d ++ ".svg","ric" ++ show d ++ ".svg"]
         | d <- [N .. NW]]

actionToFile :: Action -> FilePath
actionToFile a = case a of
  Inert -> "img/hexOn.png"
  Absorb -> "img/stop.svg"
  Stop _ -> "img/stop.svg"
  ChDir True _ d -> "img/start" ++ show d ++ ".svg"
  ChDir False _ d -> "img/ric" ++ show d ++ ".svg"
  Split _ -> "img/split.svg"
