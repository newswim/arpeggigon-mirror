{-# LANGUAGE Arrows #-}

module Reactogon.Auxiliary.Auxiliary where

import FRP.Yampa



-- Throws an Event when the incoming signal change. The Event is
-- tagged with the new value.
onChange :: (Eq a) => SF a (Event a)
onChange = proc a -> do
  b <- onChange' -< a
  returnA -< e
  where onChange' :: (Eq a) => SF (a,a) Bool
        onChange' = arr $ uncurry (==)
