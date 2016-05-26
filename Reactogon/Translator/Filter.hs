-- Contains function for scheduling and filtering events given the
-- correct informations.

module Reactogon.Translator.Filter where

import Data.Bifunctor               as BF
import Data.List                    (group, sortBy)
import Data.Ord
import FRP.Yampa
import Reactogon.Semantics
import Reactogon.Translator.Message
import Sound.JACK                   (NFrames (NFrames))

-- Takes a list of time stamped "things", a sample rate and a buffer
-- size. The function argument is a function that needs to tell which
-- arguments are kept in the case where two would come into
-- contact. On the left are the events that can be thrown into the
-- buffer, on the right are the events that will need to wait. Both
-- list are sorted.
--
-- /!\ The time is relative. A preprocessing operation removing all
-- events to soon to be happening and shifting them is necessary.
schedule :: (Eq a) =>
            SampleRate
         -> NFrames
         -> [(Time, a)]
         -> ([(NFrames,a)], [(Time,a)])
schedule sr (NFrames size) = BF.first convertTime . break ((>= maxTime) . fst)
                             . sortBy (comparing fst)
  where srd = fromIntegral sr
        maxTime = fromIntegral size / srd
        convertTime :: (Eq a) => [(Time, a)] -> [(NFrames, a)]
        convertTime = map (BF.first (NFrames . floor . (srd *)))

-- The function choose between the event in case two are in conflict.
--
-- /!\ That functional argument is a bit unsatisfying, it would be
-- probably better if we'd try to push events to the next frame if
-- they conflict and only remove them if it's impossible to do
-- otherwise.
nubDuplicate :: (Eq a) => ([a] -> a) -> [(NFrames, a)] -> [(NFrames, a)]
nubDuplicate f = map (BF.second f)
                 . map (\l@((n,_):_) -> (n,map snd l)) . group

chooseDuplicate :: [a] -> a
chooseDuplicate = undefined
