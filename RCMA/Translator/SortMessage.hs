{-# LANGUAGE Arrows #-}

-- The idea is that the stream of data coming from the MIDI input port
-- will be sorted in three categories: note on events, controller
-- events and other events. The latter will be transmitted as is
-- through the whole systems.

module RCMA.Translator.SortMessage where

import qualified Data.Bifunctor             as BF
import           Data.Function              (on)
import           Data.List                  (groupBy)
import           Data.Maybe
import           Data.Ratio
import           FRP.Yampa
import           RCMA.Semantics
import           RCMA.Translator.Controller
import           RCMA.Translator.Message
import           RCMA.Translator.Note

sortRawMessages :: [(Frames, RawMessage)]
                -> ([(Frames,Message)], [(Frames,RawMessage)])
sortRawMessages = sortRawMessages' ([],[])
  where sortRawMessages' r [] = r
        sortRawMessages' (m, rm) (x@(n,xm):xs)
          | isNothing nm = sortRawMessages' (m, x:rm) xs
          | otherwise = sortRawMessages' ((n,fromJust nm) :m, rm) xs
          where nm = fromRawMessage xm

-- Direct each message to a specific channel.
-- /!\ To be modified.
sortChannel :: [Message] -> [(Int,[Message])]
sortChannel = map ((,) <$> (fst . head) <*> (map snd))
              . groupBy ((==) `on` fst) . map sortChannel'
  where sortChannel' :: Message -> (Int, Message)
        sortChannel' m = let c = getChannel m in (c,m)

-- NoteOn messages are on the right, other Control messages are on the
-- left. For now we throw away NoteOff messages.
sortNotes :: [(Frames, Message)]
          -> ([(Frames,Message)], [(Frames,Message)])
sortNotes = sortNotes' ([],[])
  where sortNotes' r [] = r
        sortNotes' (n, c) (x@(_,m):xs)
          | isNoteOn m = sortNotes' (x:n, c) xs
          | isNoteOff m = sortNotes' (n,c) xs
          | isControl m = sortNotes' (n,x:c) xs
          | otherwise = sortNotes' (n,c) xs

-- Note messages are converted to PlayHeads
convertMessages :: ([(Frames,Message)], [(Frames,Message)])
                -> ([(Frames,Note)], [(Frames,Controller)])
convertMessages = proc (notes, ctrl) -> do
  notes' <- arr $ map (BF.second messageToNote)   -< notes
  ctrl'  <- arr $ map (BF.second messageToController) -< ctrl
  returnA -< (notes', ctrl')
