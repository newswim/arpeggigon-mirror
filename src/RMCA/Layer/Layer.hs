{-# LANGUAGE Arrows, TupleSections #-}

module RMCA.Layer.Layer where

import Data.CBMVar
import Data.Ratio
import Data.ReactiveValue
import FRP.Yampa
import RMCA.Auxiliary
import RMCA.Global.Clock
import RMCA.Semantics

-- Data representing the state of a layer. It is updated continuously.
data Layer = Layer { layerBeat   :: Rational
                   , relPitch    :: RelPitch
                   , strength    :: Strength
                   , beatsPerBar :: BeatsPerBar
                   , volume      :: Int
                   } deriving (Show,Read,Eq)

layerMetronome :: SF (Event AbsBeat, Layer) (Event BeatNo)
layerMetronome = proc (eb, Layer { layerBeat = r
                                 , beatsPerBar = bpb }) -> do
  ewbno <- accumFilter (\_ (ab,r) -> ((),selectBeat (ab,r))) () -< (,r) <$> eb
  accumBy (flip nextBeatNo) 1 -< ewbno `tag` bpb
  where selectBeat (absBeat, layBeat) =
          maybeIf ((absBeat - 1) `mod` floor (fromIntegral maxAbsBeat * layBeat) == 0)
{-
-- /!\ To be changed in the initialization of the bpb /!\
layerMetronome :: SF (Tempo, Layer) (Event BeatNo)
layerMetronome = proc (t,l@Layer { beatsPerBar = bpb }) -> do
  eb <- metronome <<< layerTempo -< (t,l)
  accumBy (flip nextBeatNo) 1 -< eb `tag` bpb
-}
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
defaultLayer = Layer { layerBeat   = 1 % 4
                     , relPitch    = 0
                     , strength    = 1
                     , beatsPerBar = 4
                     , volume      = 127
                     }
