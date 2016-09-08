{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}

module RMCA.YampaReactive where

import Data.ReactiveValue
import FRP.Yampa
import Hails.Yampa
import RMCA.IOClockworks

yampaReactiveFrom :: (ReactiveValueRead c a IO) => SF a b -> c
                  -> IO (ReactiveFieldRead IO b)
yampaReactiveFrom sf rv = do
  init <- reactiveValueRead rv
  (input,output) <- yampaReactiveDual init sf
  rv =:> input
  return output

yampaReactiveWithMetronome :: (ReactiveValueRead c a IO) =>
                              a -> SF a b -> c -> DTime
                           -> IO (ReactiveFieldRead IO b)
yampaReactiveWithMetronome init sf rv dt = do
  clock <- mkClock dt
  (input,output) <- yampaReactiveDual init sf
  rv =:> input
  reactiveValueOnCanRead clock $
    reactiveValueRead rv >>= reactiveValueWrite input
  return output

yampaReactiveWithTick :: (ReactiveValueRead c a IO) =>
                         a -> SF a b -> c -> IOTick
                      -> IO (ReactiveFieldRead IO b)
yampaReactiveWithTick init sf rv tick = do
  (input,output) <- yampaReactiveDual init sf
  rv =:> input
  reactiveValueOnCanRead tick $
    reactiveValueRead rv >>= reactiveValueWrite input
  return output
