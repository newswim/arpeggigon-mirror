import AvgIvl

import FRP.Yampa

basicList :: (Event a, [(DTime, Maybe (Event Int))])
basicList = (NoEvent,
             concat $ repeat [ (1,Just (Event 1))
                             , (1,Just (Event 1))
                             , (1, Just (Event 1))
                             , (1, Just (Event 1))
                             , (1, Just (Event 1))])

randomList :: [DTime]
randomList = embed avgIvl basicList
