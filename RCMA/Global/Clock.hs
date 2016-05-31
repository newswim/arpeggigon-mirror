module Reactogon.Global.Clock ( tempo
                              , metronome
                              , tempoToDTime
                              ) where

import Reactogon.Auxiliary.Auxiliary
import Reactogon.Semantics
import FRP.Yampa

import Debug.Trace

tempo :: Tempo -> SF () Tempo
tempo = constant

-- The initial value is arbitrary but never appears because the switch
-- is immediate.
metronome :: SF Tempo (Event Beat)
metronome = switch ((repeatedly (tempoToDTime 60) ())
                     &&&
                    (onChange')) (metronome')
  where metronome' :: Tempo -> SF Tempo (Event Beat)
        metronome' t = (switch ((repeatedly (tempoToDTime t) ())
                                 &&&
                                 onChange) (metronome'))

tempoToDTime :: Tempo -> DTime
tempoToDTime = (60/) . fromIntegral
