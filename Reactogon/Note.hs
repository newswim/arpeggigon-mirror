module Note where

import MIDI

changePitch :: (Pitch -> Pitch) -> Note -> Note
changePitch f (NoteOn  p v) = NoteOn  (f p) v
changePitch f (NoteOff p v) = NoteOff (f p) v

changeVelocity :: (Velocity -> Velocity) -> Note -> Note
changeVelocity f (NoteOn  p v) = NoteOn  p (f v)
changeVelocity f (NoteOff p v) = NoteOff p (f v)

perfectFifth :: Note -> Note
perfectFifth = changePitch (toPitch . (+7) . fromPitch)

majorThird :: Note -> Note
majorThird = changePitch (toPitch . (+4) . fromPitch)

minorThird :: Note -> Note
minorThird = changePitch (toPitch . (+3) . fromPitch)
