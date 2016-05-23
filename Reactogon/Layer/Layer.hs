{-# LANGUAGE Arrows #-}

module Reactogon.Layer.Layer where

import Reactogon.Beat
import Reactogon.Semantics
import Reactogon.Global.Clock
import FRP.Yampa

-- Data representing the state of a layer. It is updated continuously.
data Layer = Layer { relTempo :: Double
                   , strength :: Strength
                   }

layerClock :: SF () Tempo -> SF Layer Tempo
layerClock globalTempo = proc Layer { relTempo = r } -> do
  t <- tempo -< ()
  returnA -< floor $ r * fromIntegral t

layerMetronome :: a
layerMetronome = metronome layerClock

-- A layer is a producer of events triggered by the system beat clock.
layer :: SF () (Event Beat) ->  SF Layer (Event Note)
layer beatSource = undefined
