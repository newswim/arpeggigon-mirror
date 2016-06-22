{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}

module RMCA.GUI.Board where

import Data.Array
import Game.Board.BasicTurnGame
import Graphics.UI.Gtk
import RMCA.Semantics           hiding (Action)

newtype GUIBoard = GUIBoard (GameState Int Cell () Action)

boardToList :: Board -> [(Int,Int,Cell)]
boardToList = map (\((x,y),z) -> (x,y,z)) . assocs

initGUIBoard :: GUIBoard
initGUIBoard = GUIBoard $ GameState
  { curPlayer'   = ()
  , boardPos     = boardToList $ makeBoard []
  , boardPieces' = []
  }

instance Show GUIBoard where
  show _ = "lol"

instance PlayableGame GUIBoard Int Cell () Action where
  curPlayer _               = ()
  allPos (GUIBoard game)    = boardPos game
  allPieces (GUIBoard game) = boardPieces' game
  moveEnabled _             = True
  canMove _ _ _             = True
  canMoveTo _ _ _ _         = True

pixbufFrom :: (Int, Int) -> Cell -> IO Pixbuf
pixbufFrom (hexW,hexH) (a,_) = do
  let pixbufScaleSimple' p = pixbufScaleSimple p hexW hexH InterpBilinear
      actionToFile = case a of
                       Inert -> "img/hexOff.png"
                       Absorb -> "img/stop.svg"
                       Stop _ -> "img/stop.svg"
                       ChDir True _ _ -> "img/start.svg"
                       ChDir False _ _ -> "img/ric.svg"
                       Split _ -> "img/split.svg"
      pixbufComposeAct p pa =
        pixbufComposite p pa 0 0 hexW hexH 0 0 1 1 InterpBilinear 255
  pixbufOn <- pixbufScaleSimple' =<< pixbufNewFromFile "img/hexOff.png"
  pixbufAct <- pixbufScaleSimple' =<< pixbufNewFromFile actionToFile
  pixbufComposeAct pixbufOn pixbufAct
  return pixbufAct
