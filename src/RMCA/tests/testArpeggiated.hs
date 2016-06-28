import FRP.Yampa

import MIDI
import Note
import Arpeggiated

import System.IO
import Data.Maybe

ifReadyDo :: Handle -> IO a -> IO (Maybe a)
ifReadyDo hnd x = hReady hnd >>= f
   where f True = x >>= return . Just
         f _    = return Nothing

main :: IO ()
main = reactimate initInput sensing output arpeggiated

initInput = return (110, NoEvent)

sensing _ = do c <- stdin `ifReadyDo` getChar
               let c' = if isJust c
                        then Event (NoteOn (toPitch 60) (toVelocity 100))
                        else NoEvent
               return (1, Just (110,c'))

output _ x = print x >> return False
