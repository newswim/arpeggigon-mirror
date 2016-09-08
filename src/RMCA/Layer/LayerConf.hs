{-# LANGUAGE Arrows, TupleSections #-}

module RMCA.Layer.LayerConf where

import Data.Ratio
import Data.ReactiveValue
import FRP.Yampa
import RMCA.Auxiliary
import RMCA.Global.Clock
import RMCA.Semantics

-- | Datatype representing dynamically modifiable characteristics for a layer.
data DynLayerConf = DynLayerConf { layerBeat :: Rational
                                 , relPitch  :: RelPitch
                                 , strength  :: Strength
                                 } deriving (Show, Read, Eq)

-- | Datatype representing statically modifiable characteristics for a layer.
data StaticLayerConf = StaticLayerConf { beatsPerBar :: BeatsPerBar
                                       } deriving (Show, Read, Eq)

-- | Datatype containing informations useful for the synthetizer.
data SynthConf = SynthConf { volume     :: Int
                           , instrument :: InstrumentNo
                           } deriving (Show, Read, Eq)

type LayerConf = (StaticLayerConf, DynLayerConf, SynthConf)

dynConf :: LayerConf -> DynLayerConf
dynConf (_,d,_) = d

staticConf :: LayerConf -> StaticLayerConf
staticConf (s,_,_) = s

synthConf :: LayerConf -> SynthConf
synthConf (_,_,s) = s

layerMetronome :: StaticLayerConf
               -> SF (Event AbsBeat, DynLayerConf) (Event BeatNo)
layerMetronome (StaticLayerConf { beatsPerBar = bpb
                                }) =
  proc (eb, DynLayerConf { layerBeat = r
                         }) -> do
    ewbno <- accumFilter (\_ (ab,r) -> ((),selectBeat (ab,r))) () -< (,r) <$> eb
    accumBy (flip nextBeatNo) 1 -< ewbno `tag` bpb
      where selectBeat (absBeat, layBeat) =
              maybeIf ((absBeat - 1) `mod`
                        floor (fromIntegral maxAbsBeat * layBeat) == 0)

getDefaultLayerConfRV :: IO (ReactiveFieldReadWrite IO LayerConf)
getDefaultLayerConfRV = newCBMVarRW defaultLayerConf

defaultLayerConf :: LayerConf
defaultLayerConf = (defaultStaticLayerConf,defaultDynLayerConf,defaultSynthConf)

defaultStaticLayerConf :: StaticLayerConf
defaultStaticLayerConf = StaticLayerConf { beatsPerBar = 4
                                         }
defaultDynLayerConf :: DynLayerConf
defaultDynLayerConf = DynLayerConf { layerBeat = 1 % 4
                                   , relPitch = 0
                                   , strength = 1
                                   }
defaultSynthConf :: SynthConf
defaultSynthConf = SynthConf { volume = 127
                             , instrument = 0
                             }
