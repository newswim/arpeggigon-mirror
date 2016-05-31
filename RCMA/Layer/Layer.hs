{-# LANGUAGE Arrows #-}

module RCMA.Layer.Layer where

import FRP.Yampa
import RCMA.Global.Clock
import RCMA.Semantics

-- Data representing the state of a layer. It is updated continuously.
data Layer = Layer { relTempo    :: Double
                   , relPitch    :: RelPitch
                   , strength    :: Strength
                   , beatsPerBar :: BeatsPerBar
                   , beatCounter :: BeatNo
                   }

layerTempo :: SF (Tempo, Layer) Tempo
layerTempo = proc (t, Layer { relTempo = r }) -> do
  returnA -< floor $ r * fromIntegral t

-- The layer is modified after the beat as been
layerMetronome :: SF (Tempo, Layer) (Event (BeatNo, Layer))
layerMetronome = proc (t, l@Layer { beatCounter = b , beatsPerBar = bpb}) -> do
  eb <- metronome <<< layerTempo -< (t, l)
  returnA -< eb `tag` let nb = nextBeatNo b bpb in (nb, l { beatCounter = nb })

-- A layer is a producer of events triggered by the system beat clock.
layer :: SF () (Event Beat) ->  SF Layer (Event Note)
layer beatSource = undefined
