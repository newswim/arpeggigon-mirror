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
import           Game.Board.BasicTurnGame
import           Graphics.UI.Gtk                  hiding (Action)
import           Graphics.UI.Gtk.Board.BoardLink
import           Graphics.UI.Gtk.Board.TiledBoard hiding (Board)
import qualified Graphics.UI.Gtk.Board.TiledBoard as BIO
import           Paths_RMCA
import           RMCA.Global.Clock
import           RMCA.Semantics

data GUICell = GUICell { cellAction  :: Action
                       , repeatCount :: Int
                       , asPh        :: Bool
                       } deriving(Show)

newtype GUIBoard = GUIBoard { toGS :: GameState Int Tile Player GUICell }

type IOBoard = BIO.Board Int Tile (Player,GUICell)

data Tile = Tile
data Player = Player deriving(Show)

rotateGUICell :: GUICell -> GUICell
rotateGUICell g = g { cellAction = rotateAction $ cellAction g }
  where rotateAction (ChDir b na d) = ChDir b na (nextDir d)
        rotateAction x = x

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
tileH = round d
  where d :: Double
        d = sqrt 3 * fromIntegral tileW / 3

hexW :: Int
hexW = round d
  where d :: Double
        d = 4 * fromIntegral tileW / 3

hexH :: Int
hexH = round d
  where d :: Double
        d = sqrt 3 * fromIntegral hexW / 2

xMax, yMax :: Int
(xMax,yMax) = BF.second (*2) $ neighbor N nec
xMin, yMin :: Int
(xMin,yMin) = BF.second (*2) swc

boardToTile :: [(Int,Int,Tile)]
boardToTile = [(x,y,Tile) | (x,y) <- range ( (xMin-1,yMin)
                                           , (xMax+3,yMax+1))]

defNa :: NoteAttr
defNa = NoteAttr { naArt = NoAccent
                 , naDur = 1 % 4
                 , naOrn = noOrn
                 }

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

outGUIBoard :: (Int,Int) -> Bool
outGUIBoard (xf,yf) = xf < xMin || xf > xMax || yf < yMin || yf > yMax

inertCell :: GUICell
inertCell = GUICell { cellAction = Inert
                    , repeatCount = 1
                    , asPh = False
                    }

initGUIBoard :: GUIBoard
initGUIBoard = GUIBoard GameState
  { curPlayer'   = Player
  , boardPos     = boardToTile
  , boardPieces' = boardToPiece [] $ makeBoard []
  }

instance PlayableGame GUIBoard Int Tile Player GUICell where
  curPlayer _               = Player
  allPos (GUIBoard game)    = boardPos game
  allPieces (GUIBoard game) = boardPieces' game
  moveEnabled _             = True
  canMove (GUIBoard game) _ (x,y)
    | Just (_,p) <- getPieceAt game (x,y)
    , GUICell { cellAction = Inert } <- p = False
    | Nothing <- getPieceAt game (x,y) = False
    | otherwise = True
  canMoveTo _ _ _ fPos = fPos `elem` validArea
                         || outGUIBoard fPos

  move (GUIBoard game) _ iPos@(_,yi) fPos@(xf,yf)
    | outGUIBoard iPos && outGUIBoard fPos = []
    | outGUIBoard fPos = [ RemovePiece iPos
                         , AddPiece iPos Player nCell ]
    | iPos `elem` ctrlCoords = [ RemovePiece fPos'
                               , AddPiece fPos' Player
                                 (nCell { cellAction = ctrlAction }) ]
    | otherwise = [ MovePiece iPos fPos'
                  , AddPiece iPos Player nCell ]
    where fPos'
            |    (xf `mod` 2 == 0 && yf `mod` 2 == 0)
              || (xf `mod` 2 /= 0 && yf `mod` 2 /= 0) = (xf,yf)
            | otherwise = (xf,yf+signum' (yf-yi))
          signum' x
            | x == 0 = 1
            | otherwise = signum x
          ctrlAction = cellAction $ snd $ fromJust $ getPieceAt game iPos
          nCell
            | Just (_,GUICell { asPh = ph, repeatCount = n }) <-
                getPieceAt game iPos = inertCell { repeatCount = n
                                                 , asPh = ph
                                                 }
            | otherwise = inertCell

  applyChange (GUIBoard game) (AddPiece (x,y) Player piece) =
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

-- Initializes a readable RV for the board and an readable-writable RV
-- for the playheads. Also installs some handlers for pieces modification.
initBoardRV :: BIO.Board Int Tile (Player,GUICell)
            -> IO ( ReactiveFieldRead IO Board
                  , Array Pos (ReactiveFieldWrite IO GUICell)
                  , ReactiveFieldReadWrite IO [PlayHead])
initBoardRV board@BIO.Board { boardPieces = (GameBoard gArray) } = do
  -- RV creation
  phMVar <- newCBMVar []
  notBMVar <- mkClockRV 100
  let getterB :: IO Board
      getterB = do
        (boardArray :: [((Int,Int),Maybe (Player,GUICell))]) <- getAssocs gArray
        let board = makeBoard $
              map (BF.first fromGUICoords .
                   BF.second ((\(_,c) -> (cellAction c,repeatCount c)) .
                              fromJust)) $
              filter (isJust . snd) boardArray
        return board

      notifierB :: IO () -> IO ()
      notifierB = reactiveValueOnCanRead notBMVar

      getterP :: IO [PlayHead]
      getterP = readCBMVar phMVar

      setterP :: [PlayHead] -> IO ()
      setterP lph = do
        oph <- readCBMVar phMVar
        let offPh :: PlayHead -> IO ()
            offPh ph = do
              let pos = toGUICoords $ phPos ph
              piece <- boardGetPiece pos board
              when (isJust piece) $ do
                let (_,c) = fromJust piece
                boardSetPiece pos (Player, c { asPh = False }) board
            onPh :: PlayHead -> IO ()
            onPh ph =  do
              let pos = toGUICoords $ phPos ph
              piece <- boardGetPiece pos board
              when (isJust piece) $ do
                let (_,c) = fromJust piece
                boardSetPiece pos (Player, c { asPh = True }) board
        postGUIAsync $ mapM_ offPh oph
        postGUIAsync $ mapM_ onPh lph
        writeCBMVar phMVar lph

      notifierP :: IO () -> IO ()
      notifierP = installCallbackCBMVar phMVar

      b = ReactiveFieldRead getterB notifierB
      ph = ReactiveFieldReadWrite setterP getterP notifierP

      setterW :: (Int,Int) -> GUICell -> IO ()
      setterW i g = postGUIAsync $ boardSetPiece i (Player,g) board

      arrW :: Array Pos (ReactiveFieldWrite IO GUICell)
      arrW = array (minimum validArea, maximum validArea)
             [(i, ReactiveFieldWrite (setterW i))
             | i <- validArea :: [(Int,Int)]]

  return (b,arrW,ph)

    {-
  boardOnPress board
    (\i -> do
        mp <- boardGetPiece i board
        when (i `elem` validArea && isJust mp && fromJust mp == Inert) $
-}


fileToPixbuf :: IO [(FilePath,Pixbuf)]
fileToPixbuf = mapM (\f -> let f' = ("img/" ++ f) in
                        uncurry (liftM2 (,))
                        ( return f'
                        , getDataFileName f' >>=
                          (pixbufNewFromFile >=>
                           \p -> pixbufScaleSimple p hexW hexW InterpBilinear)))
               (["hexOn.png","hexOff.png","stop.svg","split.svg","absorb.svg"] ++
                concat [["start" ++ show d ++ ".svg","ric" ++ show d ++ ".svg"]
                       | d <- [N .. NW]])

actionToFile :: GUICell -> FilePath
actionToFile GUICell { cellAction = a
                     , asPh = ph
                     } =
  case a of
    Inert           -> "img/hexO" ++ (if ph then "n" else "ff") ++ ".png"
    Absorb          -> "img/absorb.svg"
    Stop _          -> "img/stop.svg"
    ChDir True _ d  -> "img/start" ++ show d ++ ".svg"
    ChDir False _ d -> "img/ric" ++ show d ++ ".svg"
    Split _         -> "img/split.svg"
