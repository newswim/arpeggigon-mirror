module ClientState where

import Sound.JACK ( NFrames
                  )

data ClientState = ClientState { rate :: Int
                               , buffSize :: NFrames
                               }
