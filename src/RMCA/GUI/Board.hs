{-# LANGUAGE FlexibleContexts, FlexibleInstances, LambdaCase,
             MultiParamTypeClasses, ScopedTypeVariables, TypeSynonymInstances
             #-}

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
import           Data.Word
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
import           Paths_arpeggigon
import           RMCA.GUI.HelpersRewrite
import           RMCA.IOClockworks
import           RMCA.Semantics

import           Debug.Trace

newtype GUIBoard = GUIBoard (GameState Int Tile Player GUICell)

-- There are two types of tiles that can be distinguished by setting
-- two different colors for debugging purposes. A future release might
-- want to remove that.
data Tile = TileW | TileB

rotateGUICell :: GUICell -> GUICell
rotateGUICell g = g { cellAction = rotateAction $ cellAction g }
  where rotateAction (ChDir b na d) = ChDir b na (nextDir d)
        rotateAction (Split na ds)  = Split na (turnQueue ds 1)
        rotateAction x              = x

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

{-
hexH :: Int
hexH = round d
  where d :: Double
        d = sqrt 3 * fromIntegral hexW / 2
-}

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
                            , AddPiece fPos Player p
                            ]
    | otherwise = guiBoard

initGame :: IO (Game GUIBoard Int Tile Player GUICell)
initGame = do
  tilePixbufB <- pixbufNew ColorspaceRgb False 8 tileW tileH
  tilePixbufW <- pixbufNew ColorspaceRgb False 8 tileW tileH
  pixbufFill tilePixbufB 50 50 50 0
  pixbufFill tilePixbufW 50 50 50 0
  pixPiece <- pixbufForPiece
  let pixTile :: Tile -> Pixbuf
      pixTile TileW = tilePixbufW
      pixTile TileB = tilePixbufB
      visualA = VisualGameAspects { tileF = pixTile
                                  , pieceF = \(_,g) -> pixPiece g
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
        -- print board
        return board

      notifierB :: IO () -> IO ()
      notifierB = reactiveValueOnCanRead tc

      getterP :: IO [PlayHead]
      getterP = readCBMVar phMVar

      setterP :: [PlayHead] -> IO ()
      setterP lph = do
        oph <- readCBMVar phMVar
        unless (oph == lph) $ do
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

-- If the repeatCount of some tile is superior to mrc,
-- then this tile will be undistinguishable from any other tile with a
-- repeat count superior to mrc.
mrc :: (Num a) => a
mrc = 6

pixbufForPiece :: IO (GUICell -> Pixbuf)
pixbufForPiece = do
  let colorPlayHead _ r g b ma = if (r == 0 && g == 0 && b == 0)
                                 then (0, 0, 0, ma)
                                 else (0, g, 0, ma)
      colorRC 0  _ _ _ _ ma = (0, 0, 0, ma)
      colorRC rc _ r g b ma =
        if (r == 0 && g == 0 && b == 0)
        then (0, 0, 0, ma)
        else let (gradr, gradg, gradb)  = ( (maxBound - r) `quot` mrc
                                          , (g - minBound) `quot` mrc
                                          , (b - minBound) `quot` mrc
                                          )
             in ( r + gradr * (rc - 1)
                , g - gradg * (rc - 1)
                , b - gradb * (rc - 1)
                , ma
                )
  pixbufs <- mapM (\(a,rc) -> do df <- getDataFileName $ actionToFile a
                                 p <- do p' <- pixbufNewFromFile df
                                         pixbufScaleSimple p' hexW hexW InterpBilinear
                                 modifyPixbuf (colorRC rc) p
                                 p' <- pixbufCopy p
                                 modifyPixbuf colorPlayHead p'
                                 return ((a,rc), (p, p'))
                  ) [(a,r) | r <- [0..mrc], a <- actionList]
  let f GUICell { cellAction = a
                , asPh = t
                , repeatCount = r } =
        (if t then snd else fst) $ fromJust $
        lookup (anonymizeConstructor a, min (fromIntegral r) mrc) pixbufs
  return f

modifyPixbuf :: ((Int, Int) -> Word8 ->  Word8 -> Word8 -> Maybe Word8 ->
                (Word8, Word8, Word8, Maybe Word8))
             -> Pixbuf -> IO ()
modifyPixbuf f p = do
  pixs <- pixbufGetPixels p
  w <- pixbufGetWidth p
  h <- pixbufGetHeight p
  rs <- pixbufGetRowstride p
  chans <- pixbufGetNChannels p
  forM_ [(x,y) | x <- [0..w - 1], y <- [0..h - 1]] $ \(x,y) -> do
    let p = x * rs + y * chans
    red <- readArray pixs p
    green <- readArray pixs (p + 1)
    blue <- readArray pixs (p + 2)
    alpha <- if (chans == 4)
             then fmap Just $ readArray pixs (p + 3)
             else return Nothing
    let (nr, ng, nb, na) = f (x,y) red green blue alpha
    writeArray pixs p nr
    writeArray pixs (p + 1) ng
    writeArray pixs (p + 2) nb
    when (isJust na) $ writeArray pixs (p + 3) $ fromJust na


actionToFile :: Action -> FilePath
actionToFile = \case
                    Inert           -> "img/hexOff.png"
                    Absorb          -> "img/absorb.svg"
                    Stop _          -> "img/stop.svg"
                    ChDir True _ d  -> "img/start" ++ show d ++ ".svg"
                    ChDir False _ d -> "img/ric" ++ show d ++ ".svg"
                    Split _ _       -> "img/split.svg"
