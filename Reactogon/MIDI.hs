module MIDI ( SampleRate
            , Pitch
            , toPitch
            , fromPitch
            , Velocity
            , Voice ( fromVoice
                    , toVoice
                    )
            , Note(..)
            , ControllerIdx
            , ControllerValue
            , Control
            ) where

import Sound.MIDI.Message.Channel.Voice ( fromPitch
                                        , toPitch
                                        )
import qualified Sound.MIDI.Message.Channel.Voice as Voice

type SampleRate = Int

type Pitch = Voice.Pitch
type Velocity = Voice.Velocity

class Voice a where
  fromVoice :: Voice.T -> Maybe a
  toVoice :: a -> Voice.T

data Note = NoteOn  Pitch Velocity
          | NoteOff Pitch Velocity
  deriving(Show)

instance Voice Note where
  fromVoice (Voice.NoteOn  p v) = Just $ NoteOn  p v
  fromVoice (Voice.NoteOff p v) = Just $ NoteOff p v
  fromVoice _ = Nothing
  toVoice (NoteOn  p v) = Voice.NoteOn  p v
  toVoice (NoteOff p v) = Voice.NoteOff p v

type ControllerIdx = Voice.Controller
type ControllerValue = Int

data Control = Control ControllerIdx ControllerValue

instance Voice Control where
  fromVoice (Voice.Control i v) = Just $ Control i v
  fromVoice _ = Nothing
  toVoice (Control i v) = Voice.Control i v
