{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
             ScopedTypeVariables, TypeSynonymInstances #-}

module RMCA.GUI.Board ( GUICell (..)
                      , attachGameRules
                      , initGame
                      , initBoardRV
                      , rotateGUICell
                      , inertCell
                      , toGUICoords
                      , fromGUICoords
                      , validArea
                      , Player(..)
                      , actualTile
                      ) where

import           Control.Arrow
import           Control.Monad
import           Data.Array
import           Data.Array.MArray
import           Data.Board.GameBoardIO
import           Data.CBMVar
import           Data.Maybe
import           Data.ReactiveValue
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
import           Paths_RMCA
import           RMCA.GUI.HelpersRewrite
import           RMCA.IOClockworks
import           RMCA.Semantics

newtype GUIBoard = GUIBoard { toGS :: GameState Int Tile Player GUICell }

type IOBoard = BIO.Board Int Tile (Player,GUICell)

-- There are two types of tiles that can be distinguished by setting
-- two different colors for debugging purposes. A future release might
-- want to remove that.
data Tile = TileW | TileB


rotateGUICell :: GUICell -> GUICell
rotateGUICell g = g { cellAction = rotateAction $ cellAction g }
  where rotateAction (ChDir b na d) = ChDir b na (nextDir d)
        rotateAction x = x

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

boardToTile :: [(Int,Int,Tile)]
boardToTile = [(x,y,selTile) | (x,y) <- range ( (xMin-1,yMin)
                                              , (xMax+3,yMax+1))
                             , let selTile = if even x && even y
                                                ||
                                                odd x && odd y
                                             then TileW
                                             else TileB ]



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
  canMoveTo _ _ _ fPos = fPos `elem` validArea || outGUIBoard fPos

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
  tilePixbufB <- pixbufNew ColorspaceRgb False 8 tileW tileH
  tilePixbufW <- pixbufNew ColorspaceRgb False 8 tileW tileH
  pixbufFill tilePixbufB 50 50 50 0
  pixbufFill tilePixbufW 50 50 50 0
  let pixPiece :: (Player,GUICell) -> Pixbuf
      pixPiece (_,a) = fromJust $ lookup (actionToFile a) pixbufs
      pixTile :: Tile -> Pixbuf
      pixTile TileW = tilePixbufW
      pixTile TileB = tilePixbufB
      visualA = VisualGameAspects { tileF = pixTile
                                  , pieceF = pixPiece
                                  , bgColor = (1000,1000,1000)
                                  , bg = Nothing
                                  }

  return $ Game visualA initGUIBoard

-- Initializes a readable RV for the board and an readable-writable RV
-- for the playheads. Also installs some handlers for pieces modification.
initBoardRV :: IOTick
            -> BIO.Board Int Tile (Player,GUICell)
            -> IO ( ReactiveFieldRead IO Board
                  , Array Pos (ReactiveFieldWrite IO GUICell)
                  , ReactiveFieldWrite IO [PlayHead])
initBoardRV tc board@BIO.Board { boardPieces = (GameBoard gArray) } = do
  -- RV creation
  phMVar <- newCBMVar []
  let getterB :: IO Board
      getterB = do
        (boardArray :: [((Int,Int),Maybe (Player,GUICell))]) <- getAssocs gArray
        let board = makeBoard $
              map (first fromGUICoords .
                   second ((\(_,c) -> (cellAction c,repeatCount c)) .
                              fromJust)) $
              filter (isJust . snd) boardArray
        return board

      notifierB :: IO () -> IO ()
      notifierB = reactiveValueOnCanRead tc

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

  return (b,arrW,writeOnly ph)

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
