-- Contains function for scheduling and filtering events given the
-- correct informations.

module RMCA.Translator.Filter where

import Data.Bifunctor          as BF
import Data.List               (sortBy)
import Data.Ord
import RMCA.Translator.Message

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

-- When to events are at the same frame, shift them so that they are
-- all separated by one frame. Then take every list and make sure that
-- the first frame of the next list is at least one frame after the
-- last frame of that list.
scatterEvents :: [(Frames, a)] -> [(Frames, a)]
scatterEvents (x@(n,_):(m,b):xs) = x:scatterEvents ((m',b):xs)
  where m' = m + max 0 (1 + n - m)
scatterEvents [x] = [x]
scatterEvents _ = []
