{-# LANGUAGE Arrows, FlexibleContexts #-}

module RCMA.Layer.Board ( boardSetup
                        ) where

import Control.Concurrent
import Data.ReactiveValue
import Data.Tuple
import FRP.Yampa
import Hails.Yampa
import RCMA.Auxiliary.Curry
import RCMA.Layer.Layer
import RCMA.Semantics
import RCMA.Global.Clock
import Control.Monad

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

boardSF' :: Board
         -> [PlayHead]
         -> SF (Layer, Tempo) (Event ([PlayHead], [Note]))
boardSF' board ph = proc (l, t) -> do
  (ebno, el) <- splitE ^<< layerMetronome -< (t, l)
  boardAction board -< (l, ph, ebno)

boardSF :: Board -> SF (Layer, Tempo) (Event [Note])
boardSF board = boardSF'' board []
  where boardSF'' :: Board -> [PlayHead] -> SF (Layer, Tempo) (Event [Note])
        boardSF'' board ph = switch (splitE ^<< fmap swap ^<< boardSF' board ph)
                             (\nph -> boardSF'' board nph)

boardSetup :: Board
           -> ReactiveFieldReadWrite IO Tempo
           -> ReactiveFieldReadWrite IO Layer
           -> ReactiveFieldReadWrite IO [Note]
           -> IO ()
boardSetup board tempoRV layerRV outBoardRV = do
  layer <- reactiveValueRead layerRV
  tempo <- reactiveValueRead tempoRV
  (inBoard, outBoard) <- yampaReactiveDual (layer, tempo) (boardSF board)
  let inRV =  pairRW layerRV tempoRV
  clock <- mkClockRV 10
  inRV =:> inBoard
  clock ^:> inRV
  reactiveValueOnCanRead outBoard
    (reactiveValueRead outBoard >>= reactiveValueWrite outBoardRV . event [] id)
  return ()

(^:>) :: (ReactiveValueRead a b m, ReactiveValueReadWrite c d m) =>
         a -> c -> m ()
not ^:> rv = reactiveValueOnCanRead not resync
  where resync = reactiveValueRead rv >>= reactiveValueWrite rv
