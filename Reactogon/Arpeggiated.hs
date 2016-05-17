{-# LANGUAGE Arrows #-}

module Arpeggiated where

import FRP.Yampa

import MIDI
import Note

arpeggiated :: SF (ControllerValue, Event Note) (Event Note)
arpeggiated = proc (c,n) -> do
  non   <- uncurry gate ^<< identity &&& arr (event False isOn) -< n
  non'  <- fmap perfectFifth ^<< delayEvent t                   -< non
  non'' <- fmap majorThird ^<< delayEvent t                     -< non'
  (nof',nof'')  <- makeOff *** makeOff -< (non',non'')
  -- It's assumed that the NoteOff event corresponding to n will be
  -- emitted.
  returnA -< mergeEvents [n, non, non', nof', non'', nof'']
    where onoffGap = 0.9*t
          t = 10
          makeOff = delayEvent onoffGap <<^ fmap switchOnOff
