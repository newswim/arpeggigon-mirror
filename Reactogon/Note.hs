module Note where

import MIDI

isOn :: Note -> Bool
isOn (NoteOn _ _ _) = True
isOn _ = False

isOff :: Note -> Bool
isOff = not . isOn

changePitch :: (Pitch -> Pitch) -> Note -> Note
changePitch f (NoteOn  c p v) = NoteOn  c (f p) v
changePitch f (NoteOff c p v) = NoteOff c (f p) v

changeVelocity :: (Velocity -> Velocity) -> Note -> Note
changeVelocity f (NoteOn  c p v) = NoteOn  c p (f v)
changeVelocity f (NoteOff c p v) = NoteOff c p (f v)

switchOnOff :: Note -> Note
switchOnOff (NoteOn  c p v) = NoteOff c p v
switchOnOff (NoteOff c p v) = NoteOn  c p v

perfectFifth :: Note -> Note
perfectFifth = changePitch (toPitch . (+7) . fromPitch)

majorThird :: Note -> Note
majorThird = changePitch (toPitch . (+4) . fromPitch)

minorThird :: Note -> Note
minorThird = changePitch (toPitch . (+3) . fromPitch)
