{-# LANGUAGE Arrows, FlexibleContexts #-}

module RCMA.Layer.Board where

import FRP.Yampa
import RCMA.Layer.Layer
import RCMA.Semantics
import RCMA.Auxiliary.Curry
import Data.ReactiveValue
import Hails.Yampa
import Data.Tuple

-- The state of the board is described by the list of the playheads
-- and the different actions onto the board.
-- It can then be modified discretly when a beat is received or
-- continuously when the user acts on it.
boardAction :: Board
            -> SF (Layer, [PlayHead], Event BeatNo)
                  (Event ([PlayHead], [Note]))
boardAction board = proc (Layer { relPitch    = rp
                                , strength    = s
                                , beatsPerBar = bpb
                                }, pl, ebn) -> do
  ahSF <<^ arr propEvent -< (ebn, rp, s, pl)
  where
    ahSF :: SF (Event (BeatNo, RelPitch, Strength, [PlayHead]))
               (Event ([PlayHead], [Note]))
    ahSF = arr $ fmap (uncurry4 $ advanceHeads board)
    propEvent (a,b,c,d) = if isEvent a
                          then Event (fromEvent a,b,c,d)
                          else NoEvent

boardSF :: Board -> SF (Layer, [PlayHead], Tempo) (Event ([PlayHead], [Note]))
boardSF board = proc (l, ph, t) -> do
  (ebno, el) <- splitE ^<< layerMetronome -< (t, l)
  boardAction board -< (l, ph, ebno)

boardInit :: (ReactiveValueRead tempo Tempo IO,
              ReactiveValueRead ph [PlayHead] IO) =>
             Board
          -> tempo
          -> ph
          -> ReactiveFieldReadWrite IO Layer
          -> IO ()
boardInit board tempoRV phRV layerRV = do
  layer <- reactiveValueRead layerRV
  tempo <- reactiveValueRead tempoRV
  ph  <- reactiveValueRead phRV
  (inBoard, outBoard) <- yampaReactiveDual (layer, ph, tempo) (boardSF board)
  return ()
{-
boardRun :: (ReactiveValueRead tempo Tempo IO,
             ReactiveValueRead ph [PlayHead] IO) =>
            Board
         -> tempo
         -> ph
         -> ReactiveFieldReadWrite IO Layer
         -> IO ()
boardRun board tempoRV phRV layerRV = do
  layer <- reactiveValueRead layerRV
  tempo <- reactiveValueRead tempoRV
  ph  <- reactiveValueRead phRV
  (inBoard, outBoard) <- yampaReactiveDual (layer, tempo, ph)  (boardAction board)

  return ()
-}
