{-# LANGUAGE Arrows, TupleSections #-}

module RMCA.Layer.Layer where

import Data.CBMVar
import Data.ReactiveValue
import FRP.Yampa
import RMCA.Global.Clock
import RMCA.Semantics

-- Data representing the state of a layer. It is updated continuously.
data Layer = Layer { relTempo    :: Double
                   , relPitch    :: RelPitch
                   , strength    :: Strength
                   , beatsPerBar :: BeatsPerBar
                   , volume      :: Int
                   } deriving (Show,Read)

layerTempo :: SF (Tempo, Layer) LTempo
layerTempo = proc (t, Layer { relTempo = r }) ->
  returnA -< floor $ r * fromIntegral t

-- /!\ To be changed in the initialization of the bpb /!\
layerMetronome :: SF (Tempo, Layer) (Event BeatNo)
layerMetronome = proc (t,l@Layer { beatsPerBar = bpb }) -> do
  eb <- metronome <<< layerTempo -< (t,l)
  accumBy (\bn bpb -> nextBeatNo bpb bn) 1 -< eb `tag` bpb

layerRV :: CBMVar Layer -> ReactiveFieldReadWrite IO Layer
layerRV mvar = ReactiveFieldReadWrite setter getter notifier
  where setter :: Layer ->  IO ()
        setter = writeCBMVar mvar

        getter :: IO Layer
        getter = readCBMVar mvar

        notifier :: IO () -> IO ()
        notifier = installCallbackCBMVar mvar

getDefaultLayerRV :: IO (ReactiveFieldReadWrite IO Layer)
getDefaultLayerRV = layerRV <$> newCBMVar defaultLayer

defaultLayer :: Layer
defaultLayer = Layer { relTempo    = 1
                     , relPitch    = 0
                     , strength    = 1
                     , beatsPerBar = 4
                     , volume      = 127
                     }
