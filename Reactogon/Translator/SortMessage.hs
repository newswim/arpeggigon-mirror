{-# LANGUAGE Arrows #-}

-- The idea is that the stream of data coming from the MIDI input port
-- will be sorted in three categories: note on events, controller
-- events and other events. The latter will be transmitted as is
-- through the whole systems.

module Reactogon.Translator.SortMessage where

import qualified Data.Bifunctor               as BF
import           Data.Maybe
import           Reactogon.Semantics
import           Reactogon.Translator.Message

-- TEMPORARY
data Control
--

sortRawMessages :: [RawMessage] -> ([Message], [RawMessage])
sortRawMessages = sortRawMessages' ([],[])
  where sortRawMessages' r [] = r
        sortRawMessages' (m, rm) (x:xs)
          | isNothing nm = sortRawMessages' (m, x:rm) xs
          | otherwise = sortRawMessages' ((fromJust nm) :m, rm) xs
          where nm = fromRawMessage x

-- NoteOn messages are on the right, other Control messages are on the
-- left. For now we throw away NoteOff messages.
sortNotes :: [Message] -> ([Message], [Message])
sortNotes = sortNotes' ([],[])
  where sortNotes' r [] = r
        sortNotes' (n, c) (x:xs)
          | isNoteOn x = sortNotes' (x:n, c) xs
          | isNoteOff x = sortNotes' (n,c) xs
          | isControl x = sortNotes' (n,x:c) xs
          | otherwise = sortNotes' (n,c) xs
{-
sortMessages :: [RawMessage] -> ([Note], [Control], [RawMessage])
sortMessages = (\((a,b),c) -> (a,b,c)) . BF.first sortNotes . sortRawMessages
-}

-- Note messages are converted to PlayHeads
sortMessages :: SF ([Message], [Message]) ([Note], [Control])
sortMessages = proc (notes, ctrl) -> do
  notes' <- convertNotes -< notes
  ctrl'  <- convertControl -< ctrl
  returnA -< (notes', ctrl')

gatherMessages :: ([Note], [Control], [RawMessage]) -> [Message]
gatherMessages ([], [], []) = []
gatherMessages _ = undefined

readMessages :: SF ([RawMessage]) ([Note], [Control], [RawMessages])
readMessages = undefined
