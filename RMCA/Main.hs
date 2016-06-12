{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent
import Data.ReactiveValue
import FRP.Yampa
import Hails.Yampa
import RMCA.Auxiliary.Concurrent
import RMCA.Auxiliary.RV
import RMCA.Auxiliary.RV
import RMCA.Global.Clock
import RMCA.Layer.Board
import RMCA.Layer.Layer
import RMCA.Semantics
import RMCA.Translator.Jack
import RMCA.Translator.Message
import RMCA.Translator.Translator

import Control.Monad
import Data.Ratio

boardRVIO = newCBMVarRW $
    makeBoard [((0,0),  mkCell (ChDir False na1 N)),
               ((0,1),  mkCell (ChDir True na1 SE)),
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

newTempoRV :: IO (ReactiveFieldReadWrite IO Tempo)
newTempoRV = newCBMVarRW 96

main :: IO ()
main = do
  layerRV <- getDefaultLayerRV
  boardQueue <- newCBMVarRW []
  -- Board setup
  layer <- reactiveValueRead layerRV
  tempoRV <- newTempoRV
  tempo <- reactiveValueRead tempoRV
  boardRV <- boardRVIO
  board <- reactiveValueRead boardRV
  (inBoard, outBoard) <- yampaReactiveDual (board, layer, tempo)
                         (boardSF $ startHeads board)
  let inRV = liftRW2 (bijection (\(x,y,z) -> (x,(y,z)), \(x,(y,z)) -> (x,y,z)))
             boardRV $ pairRW layerRV tempoRV
  clock <- mkClockRV 100
  clock ^:> inRV
  inRV =:> inBoard
  --reactiveValueOnCanRead outBoard (reactiveValueRead outBoard >>= print . ("Board out " ++) . show)
  reactiveValueOnCanRead outBoard $ do
    bq <- reactiveValueRead boardQueue
    ob <- reactiveValueRead $ liftR (event [] id) outBoard
    reactiveValueWrite boardQueue (bq ++ ob)
  -- /!\ To be removed.
  --reactiveValueOnCanRead outBoard (reactiveValueRead outBoard >>= print)
  putStrLn "Board started."
  -- Jack setup
  jackSetup tempoRV (constR 0) (boardQueue)
  return ()

{-jackT <- forkChild $ jackSetup (liftR2 (\t n -> (t, 0, n)) tempoRV
  boardOutRV) -}
