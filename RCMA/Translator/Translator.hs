{-# LANGUAGE Arrows #-}

module RCMA.Translator.Translator where

import qualified Data.Bifunctor              as BF
import           FRP.Yampa
import           RCMA.Auxiliary.Curry
import           RCMA.Layer.Layer
import           RCMA.Semantics
import           RCMA.Translator.Clock
import           RCMA.Translator.Controller
import           RCMA.Translator.Message
import           RCMA.Translator.Note
import           RCMA.Translator.SortMessage

-- Uses function defined in SortMessage. This is a pure function and
-- it might not need to be a signal function.
readMessages :: [(Frames,RawMessage)]
             -> ([(Frames,Note)], [(Frames,Controller)], [(Frames,RawMessage)])
readMessages = proc r -> do
  (mes, raw) <- sortRawMessages -< r
  (notes, ctrl) <- convertMessages <<< sortNotes -< mes
  returnA -< (notes, ctrl, raw)

gatherMessages :: LTempo
               -> SampleRate
               -> Int
               -> ([(Frames,Note)],[(Frames,Controller)],[(Frames,RawMessage)])
               -> [(Frames, RawMessage)]
gatherMessages layTempo sr chan = proc (notes, ctrl, raw) -> do
  notes'   <- concat <<< map (noteToMessages layTempo sr chan) -< notes
  ctrl'    <- map (BF.second controllerToMessages)             -< ctrl
  rawNotes <- map (BF.second toRawMessage)                     -< notes'
  rawCtrl  <- map (BF.second toRawMessage)                     -< ctrl'
  returnA  -< rawNotes ++ rawCtrl ++ raw
