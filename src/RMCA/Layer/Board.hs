{-# LANGUAGE Arrows #-}

module RMCA.Layer.Board where

import FRP.Yampa
import RMCA.Auxiliary
import RMCA.Layer.Layer
import RMCA.Semantics

data BoardRun = BoardStart | BoardStop deriving Eq

{-
-- The state of the board is described by the list of the playheads
-- and the different actions onto the board.
boardAction :: SF ((Board, Layer, [PlayHead]), Event BeatNo)
               (Event ([PlayHead], [Note]))
boardAction = proc ((board, Layer { relPitch    = rp
                                  , strength    = s
                                  },ph), ebno) ->
  arr $ fmap (uncurry5 advanceHeads)
  -< ebno `tag` (board, fromEvent ebno, rp, s, ph)
  --returnA -< traceShow e e

boardSF :: SF (Board, Layer, [PlayHead], Tempo) (Event ([PlayHead], [Note]))
boardSF = proc (board, l, ph, t) -> do
  ebno <- layerMetronome -< (t, l)
  boardAction -< ((board, l, ph), ebno)
-}

singleBoard :: [PlayHead]
            -> SF (Board, Layer, Event BeatNo) (Event ([PlayHead], [Note]))
singleBoard iPh = proc (board, Layer { relPitch = rp
                                     , strength = s
                                     }, ebno) ->
  accumBy advanceHeads' (iPh,[]) -< ebno `tag` (board, fromEvent ebno, rp, s)
  where advanceHeads' (ph,_) (board,bno,rp,s) = uncurry5 advanceHeads (board,bno,rp,s,ph)

boardSF :: SF (Board, Layer, Tempo, BoardRun) (Event ([PlayHead], [Note]))
boardSF = proc (board, l, t, br) -> do
  ebno <- layerMetronome -< (t,l)
  ess <- onChange -< br
  boardSwitch [] -< ((board, l, ebno), ess `tag` (br, startHeads board))

boardSwitch :: [PlayHead]
            -> SF ((Board, Layer,Event BeatNo), Event (BoardRun, [PlayHead]))
               (Event ([PlayHead],[Note]))
boardSwitch rPh = dSwitch (singleBoard rPh *** identity) fnSwitch
  where fnSwitch (BoardStart, iPh) = boardSwitch iPh
        fnSwitch (BoardStop, _) = boardSwitch []
