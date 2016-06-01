{-# LANGUAGE Arrows #-}

module RCMA.Translator.Note where

import Data.Ratio
import FRP.Yampa
import RCMA.Global.Clock
import RCMA.Layer.Layer
import RCMA.Semantics
import RCMA.Translator.Message

messageToNote :: Message -> Note
messageToNote (NoteOn _ p s) = Note { notePch = p
                                    , noteStr = s
                                    , noteDur = 1 % 4
                                    , noteOrn = noOrn
                                    }

-- noteToMessage gives a pair of two time-stamped messages. The one on
-- the left is a note message, the other a note off.
noteToMessages :: LTempo
               -> SampleRate
               -> Int -- Channel number
               -> (Frames,Note) -- Note to convert
               -> [(Frames,Message)]
noteToMessages layTempo sr chan =
  proc (t,n@Note { notePch = p
                 , noteStr = s
                 , noteDur = d
                 }) -> do
    nm <- noteOnToMessage chan -< n
    let dt = fromRational (d * (toRational $ tempoToDTime layTempo))
        dn = floor $ dt * fromIntegral sr
    returnA -< [(t,nm),(t + dn,switchOnOff nm)]

noteOnToMessage :: Int -> Note -> Message
noteOnToMessage c (Note { notePch = p
                        , noteStr = s
                        }) = NoteOn (makeChannel c) p s
