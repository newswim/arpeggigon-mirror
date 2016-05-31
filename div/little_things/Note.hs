module Note where

import MIDI

isNoteOn :: Message -> Bool
isNoteOn (NoteOn _ _ _) = True
isNoteOn _ = False

isNoteOff :: Message -> Bool
isNoteOff (NoteOff _ _ _) = True
isNoteOff _ = False

changePitch :: (Pitch -> Pitch) -> Message-> Message
changePitch f (NoteOn  c p v) = NoteOn  c (f p) v
changePitch f (NoteOff c p v) = NoteOff c (f p) v

changeVelocity :: (Velocity -> Velocity) -> Message-> Message
changeVelocity f (NoteOn  c p v) = NoteOn  c p (f v)
changeVelocity f (NoteOff c p v) = NoteOff c p (f v)

switchOnOff :: Message-> Message
switchOnOff (NoteOn  c p v) = NoteOff c p v
switchOnOff (NoteOff c p v) = NoteOn  c p v

perfectFifth :: Message-> Message
perfectFifth = changePitch (toPitch . (+7) . fromPitch)

majorThird :: Message-> Message
majorThird = changePitch (toPitch . (+4) . fromPitch)

minorThird :: Message-> Message
minorThird = changePitch (toPitch . (+3) . fromPitch)
