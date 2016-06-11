-- Contains function for scheduling and filtering events given the
-- correct informations.

module RMCA.Translator.Filter where

import Data.Bifunctor          as BF
import Data.Function           (on)
import Data.List               (group, groupBy, sortBy)
import Data.Ord
import FRP.Yampa
import RMCA.Semantics
import RMCA.Translator.Message
import Sound.JACK              (NFrames (NFrames))

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
schedule size = BF.first scatterEvents
                . break ((>= size) . fst) . sortBy (comparing fst)

{- Rendered useless
-- The function choose between the event in case two are in conflict.
--
-- /!\ That functional argument is a bit unsatisfying, it would be
-- probably better if we'd try to push events to the next frame if
-- they conflict and only remove them if it's impossible to do
-- otherwise.
nubDuplicate :: (Eq a) => ([a] -> a) -> [(Frames, a)] -> [(Frames, a)]
nubDuplicate f = map (BF.second f) . scatterEvents
                 . map (\l@((n,_):_) -> (n,map snd l)) . group
-}
-- When to events are at the same frame, shift them so that they are
-- all separated by one frame. Then take every list and make sure that
-- the first frame of the next list is at least one frame after the
-- last frame of that list.
scatterEvents :: [(Frames, a)] -> [(Frames, a)]
scatterEvents (x@(n,a):(m,b):xs) = x:scatterEvents ((m',b):xs)
  where m' = m + max 0 (1 + n - m)
scatterEvents [x] = [x]
scatterEvents _ = []
