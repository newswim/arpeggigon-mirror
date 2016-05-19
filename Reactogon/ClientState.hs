module ClientState where

import Sound.JACK ( NFrames
                  )
import FRP.Yampa

data ClientState = ClientState { rate :: Int
                               , buffSize :: NFrames
                               , clientClock :: Time
                               }
