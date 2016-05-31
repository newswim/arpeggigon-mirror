{-# LANGUAGE Arrows #-}

module RCMA.Layer.Board where

import FRP.Yampa
import RCMA.Layer.Layer
import RCMA.Semantics

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
    uncurry4 :: (a -> b -> c -> d -> e) -> (a,b,c,d) -> e
    uncurry4 f (a,b,c,d) = f a b c d
    ahSF :: SF (Event (BeatNo, RelPitch, Strength, [PlayHead]))
               (Event ([PlayHead], [Note]))
    ahSF = arr $ fmap (uncurry4 $ advanceHeads board)
    propEvent (a,b,c,d) = if isEvent a then Event (fromEvent a,b,c,d) else NoEvent
