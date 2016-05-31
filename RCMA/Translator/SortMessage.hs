{-# LANGUAGE Arrows #-}

-- The idea is that the stream of data coming from the MIDI input port
-- will be sorted in three categories: note on events, controller
-- events and other events. The latter will be transmitted as is
-- through the whole systems.

module RCMA.Translator.SortMessage where

import qualified Data.Bifunctor               as BF
import           Data.Function                (on)
import           Data.List                    (groupBy)
import           Data.Maybe
import           FRP.Yampa
import           RCMA.Semantics
import           RCMA.Translator.Message

-- TEMPORARY
data Controller = Lol
--

sortRawMessages :: [RawMessage] -> ([Message], [RawMessage])
sortRawMessages = sortRawMessages' ([],[])
  where sortRawMessages' r [] = r
        sortRawMessages' (m, rm) (x:xs)
          | isNothing nm = sortRawMessages' (m, x:rm) xs
          | otherwise = sortRawMessages' ((fromJust nm) :m, rm) xs
          where nm = fromRawMessage x

-- Direct each message to a specific channel.
sortChannel :: [Message] -> [(Int,[Message])]
sortChannel = map ((,) <$> (fst . head) <*> (map snd))
              . groupBy ((==) `on` fst) . map sortChannel'
  where sortChannel' :: Message -> (Int, Message)
        sortChannel' m = let c = getChannel m in (c,m)

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
sortMessages :: SF ([Message], [Message]) ([Note], [Controller])
sortMessages = proc (notes, ctrl) -> do
  notes' <- arr $ map convertNotes   -< notes
  ctrl'  <- arr $ map convertControl -< ctrl
  returnA -< (notes', ctrl')

-- /!\ Unsafe function that shouldn't be exported.
convertNotes :: Message -> Note
convertNotes = undefined

-- /!\ Unsafe function that shouldn't be exported.
convertControl :: Message -> Controller
convertControl _ = Lol

gatherMessages :: ([Note], [Controller], [RawMessage]) -> [Message]
gatherMessages ([], [], []) = []
gatherMessages _ = undefined

readMessages :: SF ([RawMessage]) ([Note], [Controller], [RawMessage])
readMessages = undefined
