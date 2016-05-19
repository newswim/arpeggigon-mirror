{-# LANGUAGE Arrows #-}

module Reactimation where

import Data.Map ( Map
                , empty
                )
import qualified Data.Map as M
import FRP.Yampa
import Control.Concurrent.MVar
import Sound.JACK ( NFrames(NFrames)
                  )

import MIDI
import ClientState
--import Arpeggiated

mainReact :: MVar EventQueue
          -> MVar EventQueue
          -> MVar ClientState
          -> IO ()
mainReact inRef outRef clientRef =
  reactimate (initialize inRef) (sensing clientRef inRef) (actuation outRef) mainSF

initialize :: MVar EventQueue -> IO EventQueue
initialize inRef = readMVar inRef

sensing :: MVar ClientState
        -> MVar EventQueue
        -> Bool
        -> IO (DTime, Maybe EventQueue)
sensing clientRef inRef _ = do
  client <- readMVar clientRef
  input <- takeMVar inRef
  let (NFrames buff) = buffSize client
      dt = (fromIntegral $ rate client)/(fromIntegral buff)
  return (dt, Just input)

actuation :: MVar EventQueue
          -> Bool
          -> EventQueue
          -> IO Bool
actuation outRef _ output = do
  out <- takeMVar outRef
  putMVar outRef $ M.union output out
  return True

mainSF :: SF EventQueue EventQueue
mainSF = identity
