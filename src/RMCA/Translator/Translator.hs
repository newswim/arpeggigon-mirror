{-# LANGUAGE Arrows #-}

module RMCA.Translator.Translator where

import           Control.Arrow
import           Data.Function           (on)
import qualified Data.IntMap             as M
import           Data.List               (groupBy, sortBy)
import           Data.Maybe
import           Data.Ord
import           Data.Ratio
import           FRP.Yampa
import           RMCA.Global.Clock
import           RMCA.Semantics
import           RMCA.Translator.Message

messageToNote :: Message -> Note
messageToNote (NoteOn _ p s) = Note { notePch = p
                                    , noteStr = s
                                    , noteDur = 1 % 4
                                    , noteOrn = noOrn
                                    }
messageToNote m = error $ "In messageToNote: the message "
                  ++ show m ++ " is not a note message"

-- noteToMessage gives a pair of two time-stamped messages. The one on
-- the left is a note message, the other a note off.
noteToMessages :: SampleRate
               -> Int -- channel number
               -> LTempo
               -> (Frames,Note)
               -> [(Frames,Message)]
noteToMessages sr chan lt (t,n@Note { noteDur = d }) =
  [(t,nm),(t + dn,switchOnOff nm)]
  where nm = noteOnToMessage chan n
        dt :: Double
        dt = fromRational (d * toRational (tempoToQNoteIvl lt))
        dn = floor $ dt * fromIntegral sr

noteOnToMessage :: Int -> Note -> Message
noteOnToMessage c Note { notePch = p
                       , noteStr = s
                       } = NoteOn (mkChannel c) p s

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
sortChannel = map ((,) <$> (fst . head) <*> map snd)
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
convertMessages :: [(Frames,Message)] -> [(Frames,Note)]
convertMessages = map (second messageToNote)


-- Uses function defined in SortMessage. This is a pure function and
-- it might not need to be a signal function.
readMessages' :: [(Frames,RawMessage)]
              -> ([(Frames,Note)], [(Frames,Message)], [(Frames,RawMessage)])
readMessages' = proc r -> do
  (mes, raw) <- sortRawMessages -< r
  (notes, ctrl) <- first convertMessages <<< sortNotes -< mes
  returnA -< (notes, ctrl, raw)

readMessages :: SF [(Frames, RawMessage)]
                   ([(Frames,Note)], [(Frames,Message)], [(Frames,RawMessage)])
readMessages = arr readMessages'
{-
gatherMessages' :: LTempo
                -> SampleRate
                -> Int
                -> ([(Frames,Note)],[(Frames,Message)],[(Frames,RawMessage)])
                -> [(Frames, RawMessage)]
gatherMessages' layTempo sr chan = proc (notes, ctrl, raw) -> do
  notes'   <- concat <<< map (noteToMessages layTempo sr chan) -< notes
  rawNotes <- map (second toRawMessage)                     -< notes'
  rawCtrl  <- map (second toRawMessage)                     -< ctrl
  returnA  -< rawNotes ++ rawCtrl ++ raw

gatherMessages :: SF
  ( LTempo, SampleRate, Int
  , ([(Frames,Note)],[(Frames,Message)],[(Frames,RawMessage)]))
  [(Frames, RawMessage)]
gatherMessages = arr $ uncurry4 gatherMessages'
-}

gatherMessages :: SampleRate
               -> M.IntMap ([(LTempo,Note)],[Message])
               -> M.IntMap [(Frames,RawMessage)]
gatherMessages sr = M.map (map (second toRawMessage)) . M.mapWithKey gatherMessages'
  where gatherMessages' :: Int -> ([(LTempo,Note)],[Message])
                        -> [(Frames,Message)]
        gatherMessages' chan (notes,messages) =
          zip (repeat 0) messages ++
          concatMap (\(lt,n) -> noteToMessages sr chan lt (0,n)) notes

-- Takes a list of time stamped "things", a sample rate and a buffer
-- size. The function argument is a function that needs to tell which
-- arguments are kept in the case where two would come into
-- contact. On the left are the events that can be thrown into the
-- buffer, on the right are the events that will need to wait. Both
-- list are sorted.
--
-- /!\ The frame number is relative. A preprocessing operation
-- removing all events too soon to be happening and shifting them is
-- necessary.
schedule :: Frames
         -> [(Frames, a)]
         -> ([(Frames,a)], [(Frames,a)])
schedule size = {-first scatterEvents
                . -}break ((>= size) . fst) . sortBy (comparing fst)

-- When to events are at the same frame, shift them so that they are
-- all separated by one frame. Then take every list and make sure that
-- the first frame of the next list is at least one frame after the
-- last frame of that list.
scatterEvents :: [(Frames, a)] -> [(Frames, a)]
scatterEvents (x@(n,_):(m,b):xs) = x:scatterEvents ((m',b):xs)
  where m' = m + max 0 (1 + n - m)
scatterEvents [x] = [x]
scatterEvents _ = []
