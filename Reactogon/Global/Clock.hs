module Reactogon.Global.Clock where

import Reactogon.Semantics
import FRP.Yampa

tempo :: SF () Tempo
tempo = constant 96

metronome :: SF Tempo (Event Beat)
metronome = undefined
