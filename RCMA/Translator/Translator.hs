{-# LANGUAGE Arrows #-}

module RCMA.Translator.Translator where

import qualified Data.Bifunctor              as BF
import           FRP.Yampa
import           RCMA.Semantics
import           RCMA.Translator.Message
import           RCMA.Translator.SortMessage

-- Uses function defined in SortMessage. This is a pure function and
-- it might not need to be a signal function.
readMessages :: [(Frames,RawMessage)]
             -> ([(Frames,Note)], [(Frames,Controller)], [(Frames,RawMessage)])
readMessages = proc r -> do
  (mes, raw) <- sortRawMessages -< r
  (notes, ctrl) <- convertMessages <<< sortNotes -< mes
  returnA -< (notes, ctrl, raw)

gatherMessages :: ([(Frames,Note)], [(Frames,Controller)], [(Frames,RawMessage)])
               -> [(Frames, RawMessage)]
gatherMessages = proc (notes, ctrl, raw) -> do
  rawNotes <- map (BF.second toRawMessage) -< notes
  rawCtrl <- map (BF.second toRawMessage) -< ctrl
  returnA -< rawNotes ++ rawCtrl ++ raw
