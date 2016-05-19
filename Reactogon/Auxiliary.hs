module Auxiliary ( breakMap
                 )where

import Control.Arrow
import Data.Map (Map)
import qualified Data.Map as M

dupl :: (Arrow a) => a b c -> a (b,b) (c,c)
dupl f = f *** f

breakMap :: (Ord k) => k -> Map k a -> (Map k a, Map k a)
breakMap k m = (smaller, larger')
  where (smaller, maybeValue, larger) = M.splitLookup k m
        larger' = maybe larger (\v -> M.insert k v larger) maybeValue
