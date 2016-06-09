{-# LANGUAGE Arrows #-}

module RCMA.Layer.Layer where

import Data.CBMVar
import Data.ReactiveValue
import FRP.Yampa
import RCMA.Global.Clock
import RCMA.Semantics

import Debug.Trace

-- Data representing the state of a layer. It is updated continuously.
data Layer = Layer { relTempo    :: Double
                   , relPitch    :: RelPitch
                   , strength    :: Strength
                   , beatsPerBar :: BeatsPerBar
                   } deriving (Show)

layerTempo :: SF (Tempo, Layer) LTempo
layerTempo = proc (t, Layer { relTempo = r }) ->
  returnA -< floor $ r * fromIntegral t

-- The layer is modified after the beat as been
layerMetronome' :: BeatNo -> SF (Tempo, Layer) (Event BeatNo)
layerMetronome' b = proc (t, l@Layer { beatsPerBar = bpb }) -> do
  eb <- metronome <<< layerTempo -< (t, l)
  returnA -< eb `tag` nextBeatNo bpb b

layerMetronome :: SF (Tempo, Layer) (Event BeatNo)
layerMetronome = layerMetronome'' 0
  where layerMetronome'' no = switch (layerMetronome' no >>^ dup)
                              layerMetronome''

layerRV :: CBMVar Layer -> ReactiveFieldReadWrite IO Layer
layerRV mvar = ReactiveFieldReadWrite setter getter notifier
  where setter :: Layer ->  IO ()
        setter = writeCBMVar mvar

        getter :: IO Layer
        getter = readCBMVar mvar

        notifier :: IO () -> IO ()
        notifier = installCallbackCBMVar mvar

getDefaultLayerRV :: IO (ReactiveFieldReadWrite IO Layer)
getDefaultLayerRV = layerRV <$> newCBMVar dl
  where dl = Layer { relTempo = 1
                   , relPitch = 0
                   , strength = 127
                   , beatsPerBar = 4
                   }
