module RMCA.GUI.Board where

import Game.Board.BasicTurnGame

instance PlayableGame Board Int BoardTile () Cell where
  -- Only one player
  curPlayer _ = ()
  allPos = (\((x,y),z) -> (x,y,z)) . assoc


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
