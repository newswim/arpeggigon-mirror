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
--
-- For now this is only a tuple but a list will probably be necessary.
noteToMessages :: Tempo -> Layer -> Int -> (Time,Note)
              -> ((Time,Message),(Time,Message))
noteToMessages tempo l@(Layer { relTempo = rt }) chan =
  proc m@(t,n@Note { notePch = p
                   , noteStr = s
                   , noteDur = d
                   , noteOrn = noOrn
                   }) -> do
    nm <- noteOnToMessage l chan -< n
    t' <- returnA -< t + fromRational $ d * tempoToDTime $ rt * fromIntegral tempo
    returnA -< ((t,nm),(t',switchOnOff nm))

noteOnToMessage :: Int -> Note -> Message
noteOnToMessage c (Note { notePch = p
                        , noteStr = s
                        }) = NoteOn (makeChannel c) p s

convertControl :: Message -> Controller
convertControl _ = Lol
