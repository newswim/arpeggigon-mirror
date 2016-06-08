{-# LANGUAGE Arrows #-}

module RCMA.Layer.Layer where

import Data.CBMVar
import Data.ReactiveValue
import FRP.Yampa
import RCMA.Global.Clock
import RCMA.Semantics

-- Data representing the state of a layer. It is updated continuously.
data Layer = Layer { relTempo    :: Double
                   , relPitch    :: RelPitch
                   , strength    :: Strength
                   , beatsPerBar :: BeatsPerBar
                   , beatCounter :: BeatNo
                   } deriving (Show)

layerTempo :: SF (Tempo, Layer) LTempo
layerTempo = proc (t, Layer { relTempo = r }) -> do
  returnA -< floor $ r * fromIntegral t

-- The layer is modified after the beat as been
layerMetronome :: SF (Tempo, Layer) (Event (BeatNo, Layer))
layerMetronome = proc (t, l@Layer { beatCounter = b , beatsPerBar = bpb}) -> do
  eb <- metronome <<< layerTempo -< (t, l)
  returnA -< eb `tag` let nb = nextBeatNo b bpb in (nb, l { beatCounter = nb })

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
                   , beatCounter = 0
                   }
