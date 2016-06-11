{-# LANGUAGE Arrows #-}

module RMCA.Translator.Translator ( readMessages
                                  , gatherMessages
                                  ) where

import qualified Data.Bifunctor              as BF
import           FRP.Yampa
import           RMCA.Auxiliary.Curry
import           RMCA.Layer.Layer
import           RMCA.Semantics
import           RMCA.Translator.Controller
import           RMCA.Translator.Message
import           RMCA.Translator.Note
import           RMCA.Translator.SortMessage

-- Uses function defined in SortMessage. This is a pure function and
-- it might not need to be a signal function.
readMessages' :: [(Frames,RawMessage)]
              -> ([(Frames,Note)], [(Frames,Controller)], [(Frames,RawMessage)])
readMessages' = proc r -> do
  (mes, raw) <- sortRawMessages -< r
  (notes, ctrl) <- convertMessages <<< sortNotes -< mes
  returnA -< (notes, ctrl, raw)

readMessages :: SF [(Frames, RawMessage)]
                ([(Frames,Note)], [(Frames,Controller)], [(Frames,RawMessage)])
readMessages = arr readMessages'

gatherMessages' :: LTempo
                -> SampleRate
                -> Int
                -> ([(Frames,Note)],[(Frames,Controller)],[(Frames,RawMessage)])
                -> [(Frames, RawMessage)]
gatherMessages' layTempo sr chan = proc (notes, ctrl, raw) -> do
  notes'   <- concat <<< map (noteToMessages layTempo sr chan) -< notes
  ctrl'    <- map (BF.second controllerToMessages)             -< ctrl
  rawNotes <- map (BF.second toRawMessage)                     -< notes'
  rawCtrl  <- map (BF.second toRawMessage)                     -< ctrl'
  returnA  -< rawNotes ++ rawCtrl ++ raw

gatherMessages :: SF
  ( LTempo, SampleRate, Int
  , ([(Frames,Note)],[(Frames,Controller)],[(Frames,RawMessage)]))
  [(Frames, RawMessage)]
gatherMessages = arr $ uncurry4 gatherMessages'
