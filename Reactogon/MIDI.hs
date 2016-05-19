module MIDI ( EventQueue
            , SampleRate
            , Pitch
            , toPitch
            , fromPitch
            , fromVelocity
            , toVelocity
            , Velocity
            , Message ( NoteOn
                      , NoteOff
                      , Control
                      )
            , fromRawMessage
            , toRawMessage
            , ControllerIdx
            , ControllerValue
            ) where

import qualified Sound.MIDI.Message as Message

import Sound.MIDI.Message.Channel.Voice ( fromPitch
                                        , toPitch
                                        , fromVelocity
                                        , toVelocity
                                        )
import qualified Sound.MIDI.Message.Channel as Channel
import qualified Sound.MIDI.Message.Channel.Voice as Voice
import Data.Map (Map)
import FRP.Yampa

type EventQueue = Map Time Message

type SampleRate = Int

type RawMessage = Message.T

{-
class Message a where
  fromMessage :: RawMessage -> Maybe a
  toMessage :: a -> RawMessage
-}

type MidiVoice = Voice.T

type Channel = Channel.Channel
type Pitch = Voice.Pitch
type Velocity = Voice.Velocity

{-
class (Message a) => Voice a where
  fromVoice :: MidiVoice -> Maybe a
  toVoice :: a -> MidiVoice
-}

type ControllerIdx = Voice.Controller
type ControllerValue = Int

data Message = NoteOn  Channel Pitch Velocity
             | NoteOff Channel Pitch Velocity
             | Control Channel ControllerIdx ControllerValue
  deriving(Show)

fromRawMessage :: RawMessage -> Maybe Message
fromRawMessage (Message.Channel (Channel.Cons c
                                 (Channel.Voice (Voice.NoteOn  p v)))) = Just $ NoteOn  c p v
fromRawMessage (Message.Channel (Channel.Cons c
                                 (Channel.Voice (Voice.NoteOff p v)))) = Just $ NoteOff c p v
fromRawMessage _ = Nothing

toRawMessage :: Message -> RawMessage
toRawMessage (NoteOn  c p v) = (Message.Channel $ Channel.Cons c
                               (Channel.Voice $ Voice.NoteOn  p v))
toRawMessage (NoteOff c p v) = (Message.Channel $ Channel.Cons c
                               (Channel.Voice $ Voice.NoteOff p v))

{-
instance Message Note where
  fromMessage (Message.Channel (Channel.Cons c
               (Channel.Voice (Voice.NoteOn  p v)))) = Just $ NoteOn  c p v
  fromMessage (Message.Channel (Channel.Cons c
               (Channel.Voice (Voice.NoteOff p v)))) = Just $ NoteOff c p v
  fromMessage _ = Nothing
  toMessage (NoteOn  c p v) = (Message.Channel $ Channel.Cons c
                               (Channel.Voice $ Voice.NoteOn  p v))
  toMessage (NoteOff c p v) = (Message.Channel $ Channel.Cons c
                               (Channel.Voice $ Voice.NoteOff p v))
{-
instance Voice Note where
  fromVoice (Voice.NoteOn  p v) = Just $ NoteOn  p v
  fromVoice (Voice.NoteOff p v) = Just $ NoteOff p v
  fromVoice _ = Nothing
  toVoice (NoteOn  p v) = Voice.NoteOn  p v
  toVoice (NoteOff p v) = Voice.NoteOff p v
-}
-}
{-

data Control = Control ControllerIdx ControllerValue
-}
{-
instance Voice Control where
  fromVoice (Voice.Control i v) = Just $ Control i v
  fromVoice _ = Nothing
  toVoice (Control i v) = Voice.Control i v
-}
