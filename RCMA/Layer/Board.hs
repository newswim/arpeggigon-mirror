{-# LANGUAGE Arrows, FlexibleContexts #-}

module RCMA.Layer.Board ( boardSF
                        , (^:>)
                        ) where

import Control.Concurrent
import Control.Concurrent.MVar
import Data.ReactiveValue
import Data.Tuple
import FRP.Yampa
import Hails.Yampa
import RCMA.Auxiliary.Curry
import RCMA.Layer.Layer
import RCMA.Semantics
import RCMA.Global.Clock
import Control.Monad

import Debug.Trace

-- The state of the board is described by the list of the playheads
-- and the different actions onto the board.
-- It can then be modified discretly when a beat is received or
-- continuously when the user acts on it.
boardAction :: SF (Board, Layer, [PlayHead], Event BeatNo)
                  (Event ([PlayHead], [Note]))
boardAction = proc (board, Layer { relPitch    = rp
                                 , strength    = s
                                 , beatsPerBar = bpb
                                 }, pl, ebn) ->
  ahSF <<^ arr propEvent -< (board, ebn, rp, s, pl)
  where
    ahSF :: SF (Event (Board, BeatNo, RelPitch, Strength, [PlayHead]))
               (Event ([PlayHead], [Note]))
    ahSF = arr $ fmap (uncurry5 $ advanceHeads)
    propEvent (a,b,c,d,e) = if let a = b in traceShow a $ isEvent b
                            then Event (a,fromEvent b,c,d,e)
                            else NoEvent

boardSF :: SF (Event BeatNo) (Event ([PlayHead], [Note]))

boardSF' :: [PlayHead]
         -> SF (Board, Layer, Tempo) (Event ([PlayHead], [Note]))
boardSF' ph = proc (board, l, t) -> do
  ebno <- layerMetronome -< (t, l)
  boardAction -< (board, l, ph, ebno)

boardSF :: SF (Board, Layer, Tempo) (Event [Note])
boardSF = boardSF'' []
  where boardSF'' :: [PlayHead] -> SF (Board, Layer, Tempo) (Event [Note])
        boardSF'' ph = switch (splitE ^<< fmap swap ^<< boardSF' ph)
                       boardSF''
{-
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
  putStrLn "Board started."
  n <- newEmptyMVar
  takeMVar n
  return ()
-}
(^:>) :: (ReactiveValueRead a b m, ReactiveValueReadWrite c d m) =>
         a -> c -> m ()
not ^:> rv = reactiveValueOnCanRead not resync
  where resync = reactiveValueRead rv >>= reactiveValueWrite rv
