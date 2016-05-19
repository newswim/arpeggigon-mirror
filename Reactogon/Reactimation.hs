{-# LANGUAGE Arrows #-}

module Reactimation where

import Data.Map (Map)
import qualified Data.Map as M
import FRP.Yampa
import Control.Concurrent.MVar

import MIDI
import Arpeggiated

mainReact :: IO ()
mainReact = reactimate (initialize inRef) (sensing synthRef) actuation mainSF

initialize :: MVar EventQueue -> IO EventQueue
initialize = readMVar

sensing :: MVar (SynthState)
        -> MVar (Map Time a)
        -> Bool
        -> IO (DTime, Maybe (Map Time a))
sensing synthRef inRef _ = do
  input <- readMVar inref
  synth <- readMVar synthRef
  let dt = (fromIntegral $ rate synth)/(fromIntegral $ outBuffSize synth)
  return (dt, Just input)


actuation = undefined

mainSF :: (Message a) => SF (Map Time a) (Map Time a)
mainSF = identity
