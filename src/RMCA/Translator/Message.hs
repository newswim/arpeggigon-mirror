module RMCA.Translator.Message where

import           RMCA.Semantics
import qualified Sound.MIDI.Message               as Message
import qualified Sound.MIDI.Message.Channel       as Channel
import qualified Sound.MIDI.Message.Channel.Voice as Voice

-- We might want to move that to Semantics.
type SampleRate = Int

type RawMessage = Message.T

type MidiVoice = Voice.T

type Channel = Channel.Channel

type ControllerIdx = Voice.Controller

type Frames = Int

-- Each channel is linked to a particular translation signal function
-- itself linked to a particular layer. Therefore we will dispose of
-- the channel number as soon as possible.
---
-- /!\ This is dangerous as it only treats unipolar control values.
data Message = NoteOn  Channel Pitch Strength
             | NoteOff Channel Pitch Strength
             | Instrument Channel Voice.Program
             | Control Channel ControllerIdx UCtrl
  deriving(Show)

getChannel :: Message -> Int
getChannel (NoteOn c _ _) = Channel.fromChannel c
getChannel (NoteOff c _ _) = Channel.fromChannel c
getChannel (Control c _ _) = Channel.fromChannel c
getChannel (Instrument c _ ) = Channel.fromChannel c

mkChannel :: Int -> Channel
mkChannel = Channel.toChannel

mkProgram :: Int -> Channel.Program
mkProgram = Channel.toProgram

-- Function to go back and forth with the representations of pitches,
-- as they are different in our model and in the Jack API model.
fromRawPitch :: Voice.Pitch -> Pitch
fromRawPitch p = Pitch $ Voice.fromPitch p

toRawPitch :: Pitch -> Voice.Pitch
toRawPitch (Pitch p) = Voice.toPitch p


isNoteOn :: Message -> Bool
isNoteOn NoteOn {} = True
isNoteOn _ = False

isNoteOff :: Message -> Bool
isNoteOff NoteOff {} = True
isNoteOff _ = False

isControl :: Message -> Bool
isControl Control {} = True
isControl _ = False

switchOnOff :: Message -> Message
switchOnOff (NoteOn  c p v) = NoteOff c p v
switchOnOff (NoteOff c p v) = NoteOn  c p v
switchOnOff m = error $ "The message " ++ show m ++ " is not a note message"

fromRawMessage :: RawMessage -> Maybe Message
fromRawMessage (Message.Channel (Channel.Cons c
                                 (Channel.Voice (Voice.NoteOn  p v)))) =
  Just $ NoteOn  c (fromRawPitch p) (toUCtrl $ Voice.fromVelocity v)
fromRawMessage (Message.Channel (Channel.Cons c
                                 (Channel.Voice (Voice.NoteOff p v)))) =
  Just $ NoteOff c (fromRawPitch p) (toUCtrl $ Voice.fromVelocity v)
fromRawMessage (Message.Channel (Channel.Cons c
                                 (Channel.Voice (Voice.Control n v)))) =
  Just $ Control c n (toUCtrl v)
fromRawMessage (Message.Channel (Channel.Cons c
                                 (Channel.Voice (Voice.ProgramChange p)))) =
  Just $ Instrument c p
fromRawMessage _ = Nothing

toRawMessage :: Message -> RawMessage
toRawMessage (NoteOn  c p v) =
  Message.Channel $ Channel.Cons c
  (Channel.Voice $ Voice.NoteOn  (toRawPitch p) (Voice.toVelocity $ fromUCtrl v))
toRawMessage (NoteOff c p v) =
  Message.Channel $ Channel.Cons c
  (Channel.Voice $ Voice.NoteOff (toRawPitch p) (Voice.toVelocity $ fromUCtrl v))
toRawMessage (Control c n v) =
  Message.Channel (Channel.Cons c
                    (Channel.Voice (Voice.Control n (fromUCtrl v))))
toRawMessage (Instrument c p) =
  Message.Channel (Channel.Cons c
                    (Channel.Voice (Voice.ProgramChange p)))
