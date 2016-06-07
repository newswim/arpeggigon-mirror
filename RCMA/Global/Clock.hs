module RCMA.Global.Clock ( tempo
                         , metronome
                         , tempoToDTime
                         ) where

import FRP.Yampa
import RCMA.Auxiliary.Auxiliary
import RCMA.Semantics

tempo :: Tempo -> SF () Tempo
tempo = constant

-- The initial value is arbitrary but never appears because the switch
-- is immediate.
metronome :: SF Tempo (Event Beat)
metronome = switch ((repeatedly (tempoToDTime 60) ())
                     &&&
                    (onChange')) (metronome')
  where metronome' :: Tempo -> SF Tempo (Event Beat)
        metronome' t = (switch ((repeatedly (4 * tempoToDTime t) ())
                                 &&&
                                 onChange) (metronome'))

-- Tempo is the number of whole notes per minute.
tempoToDTime :: Tempo -> DTime
tempoToDTime = (15/) . fromIntegral
