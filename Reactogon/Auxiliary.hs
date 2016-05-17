module Auxiliary ( dupl
                 )where

import Control.Arrow

dupl :: (Arrow a) => a b c -> a (b,b) (c,c)
dupl f = f *** f
