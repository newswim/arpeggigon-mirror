{-# LANGUAGE Arrows, FlexibleContexts #-}

module RMCA.Layer.Board ( boardSF
                        , (^:>)
                        ) where

import Control.Concurrent
import Control.Concurrent.MVar
import Data.ReactiveValue
import Data.Tuple
import FRP.Yampa
import Hails.Yampa
import RMCA.Auxiliary.Curry
import RMCA.Layer.Layer
import RMCA.Semantics
import RMCA.Global.Clock
import Control.Monad

import Debug.Trace

-- The state of the board is described by the list of the playheads
-- and the different actions onto the board.
boardAction :: [PlayHead]
            -> SF ((Board, Layer), Event BeatNo) (Event ([PlayHead], [Note]))
boardAction ph = proc ((board, Layer { relPitch    = rp
                                     , strength    = s
                                     , beatsPerBar = bpb
                                     }), ebno) -> do
  e <- arr $ fmap (uncurry5 $ advanceHeads)
       -< ebno `tag` (board, fromEvent ebno, rp, s, ph)
  returnA -< traceShow e e
{-
boardSF :: SF (Board, Layer, Tempo) (Event [Note])
boardSF = proc (board, l, t) -> do
  ebno <- layerMetronome -< (t, l)
  iph <- startHeads -< board
  boardSF' iph -< (board, l, ebno)
  where boardSF' :: [PlayHead] -> SF (Board, Layer, Event BeatNo) (Event [Note])
        boardSF' ph = switch (swap ^<< splitE ^<< boardAction ph)
                      boardSF'
-}

-- We need the list of initial playheads
boardSF :: [PlayHead] -> SF (Board, Layer, Tempo) (Event [Note])
boardSF iph = proc (board, l@Layer { relPitch = rp
                                   , strength = s
                                   }, t) -> do
  ebno <- layerMetronome -< (t,l)
  --iph <- arr startHeads -< board
  boardSF' iph -< ((board, l), ebno)
  where
    boardSF' :: [PlayHead] -> SF ((Board, Layer), Event BeatNo) (Event [Note])
    boardSF' ph = switch (boardAction ph >>> arr splitE >>> arr swap)
                  (\nph -> second notYet >>> boardSF' nph)


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
