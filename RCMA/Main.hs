{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent
import Data.ReactiveValue
import FRP.Yampa
import Hails.Yampa
import RCMA.Auxiliary.Concurrent
import RCMA.Global.Clock
import RCMA.Layer.Board
import RCMA.Layer.Layer
import RCMA.Semantics
import RCMA.Translator.Jack
import RCMA.Translator.Message
import RCMA.Translator.Translator

import Control.Monad
import Data.Ratio

board =
    makeBoard [((0,0),  mkCell (ChDir False na1 N)),
               ((0,1),  mkCell (ChDir False na1 SE)),
               ((1,1),  mkCell (Split na1)),
               ((1,-1), mkCell (Split na1)),
               ((-1,0), mkCell (ChDir False na2 NE))]

na1 = NoteAttr {
          naArt = Accent13,
          naDur = 1 % 4,
          naOrn = Ornaments Nothing [] NoSlide
      }

na2 = NoteAttr {
          naArt = NoAccent,
          naDur = 1 % 16,
          naOrn = Ornaments Nothing [(10, MIDICVRnd)] SlideUp
      }

na3 = NoteAttr {
          naArt = Accent13,
          naDur = 0,
          naOrn = Ornaments Nothing [] NoSlide
      }


bpb :: Int
bpb = 4

tempoRV :: ReactiveFieldReadWrite IO Tempo
tempoRV = ReactiveFieldReadWrite (\_ -> return ()) (return 96) (\_ -> return ())

main :: IO ()
main = do
  layerRV <- getDefaultLayerRV
  boardInRV <- boardSetup board tempoRV layerRV
  jackT <- forkChild $ jackSetup (liftR2 (\t n -> (t, 0, n)) tempoRV boardInRV)
  return ()
