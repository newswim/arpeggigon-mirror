module Reactogon.Global.Clock where

import Reactogon.Auxiliary.Auxiliary
import Reactogon.Semantics
import FRP.Yampa

import Debug.Trace

tempo :: Tempo -> SF () Tempo
tempo = constant

-- The initial value is arbitrary but never appears because the switch
-- is immediate.
metronome :: SF () Tempo -> SF () (Event Beat)
metronome tempo = switch ((repeatedly (tempoToDTime 60) ())
                          &&&
                          (discard ^>> tempo >>> onChange')) (metronome' tempo)
  where metronome' :: SF () Tempo -> Tempo -> SF () (Event Beat)
        metronome' tempo t = (switch ((repeatedly (tempoToDTime t) ())
                                        &&&
                                       (discard ^>> tempo >>> onChange))
                             (metronome' tempo))

tempoToDTime :: Tempo -> DTime
tempoToDTime = (60/) . fromIntegral
