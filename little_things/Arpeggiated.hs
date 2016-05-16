{-# LANGUAGE Arrows #-}

module Arpeggiated where

import FRP.Yampa

import Sound.MIDI.Message as Message
import Sound.MIDI.Message.Channel as Channel
import Sound.MIDI.Message.Channel.Voice as Voice

arpeggiated :: SF (Event Voice.T) (Event Voice.T)
arpeggiated = proc m ->
  case m of
    Event m'@(Voice.NoteOn p v) -> returnA -< Event m'
    _ -> returnA -< NoEvent
