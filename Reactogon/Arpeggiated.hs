{-# LANGUAGE Arrows #-}

module Arpeggiated where

import FRP.Yampa

import MIDI
import Note

controllerValue :: Int -> SF () Int
controllerValue = undefined

arpeggiated :: SF (Event Note) (Event Note)
arpeggiated = proc n -> do
  c <- controllerValue 1 -< ()
  n' <- fmap perfectFifth ^<< delayEvent undefined -< n
  n'' <- fmap majorThird ^<< delayEvent undefined -< n'
  returnA -< mergeEvents [n, n', n'']
