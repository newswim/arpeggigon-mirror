module Main where

import FRP.Yampa
import Hails.Yampa
import RCMA.Semantics
import RCMA.Translator.Jack
import RCMA.Translator.Message
import RCMA.Translator.Translator

main :: IO ()
main = do
  (inp, out) <- yampaReactiveDual [] rcma
  return ()
