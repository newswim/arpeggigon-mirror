module Shared ( inRef
              , outRef
              , clientRef
              ) where

import ClientState
import MIDI

import Control.Concurrent.MVar
import Data.Map ( Map
                , empty
                )
import FRP.Yampa
import Sound.JACK ( NFrames
                  )

-- | MVar containing all the events given by the input port.
inRef :: IO (MVar EventQueue)
inRef = newMVar empty

-- | MVar containing all the events to be given to the output port.
outRef :: IO (MVar EventQueue)
outRef = newMVar empty

-- | MVar containing the state of the machine (JACK client and ports).
clientRef :: Int -> NFrames -> NFrames -> IO (MVar ClientState)
clientRef rate outSize inSize = newMVar $ ClientState { rate = rate
                                                      , outSize = outSize
                                                      , inSize = inSize
                                                      }
