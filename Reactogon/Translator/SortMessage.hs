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

sortNotes :: [Message] -> ([Note], [Control])
sortNotes = undefined

sortMessages :: [RawMessage] -> ([Note], [Control], [RawMessage])
sortMessages = (\((a,b),c) -> (a,b,c)) . BF.first sortNotes . sortRawMessages

gatherMessages :: ([Note], [Control], [RawMessage]) -> [Message]
gatherMessages ([], [], []) = []
gatherMessages _ = undefined
