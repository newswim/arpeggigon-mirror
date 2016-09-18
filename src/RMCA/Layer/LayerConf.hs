{-# LANGUAGE Arrows, TupleSections #-}

module RMCA.Layer.LayerConf where

import Data.Ratio
import Data.ReactiveValue
import RMCA.Auxiliary
import RMCA.Semantics
import RMCA.Translator.Message

-- | Datatype representing dynamically modifiable characteristics for a layer.
data DynLayerConf = DynLayerConf { layerBeat :: Rational
                                 , relPitch  :: RelPitch
                                 , strength  :: Strength
                                 , keepHeads :: Bool
                                 } deriving (Show, Read, Eq)

-- | Datatype representing statically modifiable characteristics for a layer.
data StaticLayerConf = StaticLayerConf { beatsPerBar :: BeatsPerBar
                                       , repeatCount :: Maybe Int
                                       } deriving (Show, Read, Eq)

-- | Datatype containing informations useful for the synthetizer.
data SynthConf = SynthConf { volume     :: Int
                           , instrument :: InstrumentNo
                           } deriving (Show, Read, Eq)

synthMessage :: Int -> SynthConf -> [Message]
synthMessage chan SynthConf { volume = v
                            , instrument = i
                            } = [ Volume (mkChannel chan) v
                                , Instrument (mkChannel chan) (mkProgram i)
                                ]

type LayerConf = (StaticLayerConf, DynLayerConf, SynthConf)

dynConf :: LayerConf -> DynLayerConf
dynConf (_,d,_) = d

staticConf :: LayerConf -> StaticLayerConf
staticConf (s,_,_) = s

synthConf :: LayerConf -> SynthConf
synthConf (_,_,s) = s

getDefaultLayerConfRV :: IO (ReactiveFieldReadWrite IO LayerConf)
getDefaultLayerConfRV = newCBMVarRW defaultLayerConf

defaultLayerConf :: LayerConf
defaultLayerConf = (defaultStaticLayerConf,defaultDynLayerConf,defaultSynthConf)

defaultStaticLayerConf :: StaticLayerConf
defaultStaticLayerConf = StaticLayerConf { beatsPerBar = 4
                                         , repeatCount = Nothing
                                         }
defaultDynLayerConf :: DynLayerConf
defaultDynLayerConf = DynLayerConf { layerBeat = 1 % 4
                                   , relPitch = 0
                                   , strength = 1
                                   , keepHeads = False
                                   }
defaultSynthConf :: SynthConf
defaultSynthConf = SynthConf { volume = 127
                             , instrument = 0
                             }
