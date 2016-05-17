module MIDI ( SampleRate
            , Pitch
            , toPitch
            , fromPitch
            , Velocity
            , Voice ( fromVoice
                    , toVoice
                    )
            , Note(..)
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
