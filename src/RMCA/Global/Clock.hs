{-# LANGUAGE MultiParamTypeClasses #-}

module RMCA.Global.Clock ( AbsBeat
                         , maxAbsBeat
                         , metronome
                         ) where

import FRP.Yampa
import RMCA.Auxiliary
import RMCA.Semantics

-- The absolute beat is the beat number of the global clock, there are
-- 16 starting from 1.
type AbsBeat = BeatNo

maxAbsBeat :: AbsBeat
maxAbsBeat = 16

-- The global system tempo beats every 16th note, each beat is tagged
-- with a beat number modulo sixteen. Each layer is then beating at
-- its own fraction, discarding the unecessary beats.
metronome :: SF Tempo (Event AbsBeat)
metronome = accumBy (\pb _ -> nextBeatNo maxAbsBeat pb) 0 <<<
            repeatedlyS' () <<^ (15*) <<^ (1/) <<^ fromIntegral
