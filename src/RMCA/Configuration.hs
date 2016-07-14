{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}

module RMCA.Configuration where

import           Data.Array
import qualified Data.Bifunctor     as BF
import           Data.ReactiveValue
import           RMCA.Layer.Layer
import           RMCA.Semantics
import           Text.Read

type InstrumentNo = Int

data BoardConf = BoardConf { confLayer :: (Layer,InstrumentNo)
                           , confBoard :: BoardInit
                           , confTempo :: Tempo
                           } deriving(Read,Show)

newtype BoardInit = BoardInit { toList :: [(Pos,Cell)] } deriving(Show,Read)

mkInit :: Board -> BoardInit
mkInit = BoardInit . filter (uncurry (&&) . BF.bimap onBoard notDef) . assocs
  where notDef (Inert,1) = False
        notDef _ = True

boardInit :: BoardInit -> Board
boardInit = makeBoard . toList

saveConfiguration :: ( ReactiveValueRead tempo Tempo IO
                     , ReactiveValueRead layer Layer IO
                     , ReactiveValueRead board Board IO
                     , ReactiveValueRead instr InstrumentNo IO) =>
                     FilePath -> tempo -> layer -> board -> instr -> IO ()
saveConfiguration fp t l b i = do
  tempo <- reactiveValueRead t
  layer <- reactiveValueRead l
  board <- reactiveValueRead b
  instr <- reactiveValueRead i
  let bc = BoardConf { confLayer = (layer,instr)
                     , confTempo = tempo
                     , confBoard = mkInit board
                     }
  writeFile fp $ show bc

loadConfiguration :: ( ReactiveValueRead tempo Tempo IO
                     , ReactiveValueRead layer Layer IO
                     , ReactiveValueRead board Board IO
                     , ReactiveValueRead instr InstrumentNo IO) =>
                     FilePath -> tempo -> layer -> board -> instr -> IO ()
loadConfiguration fp t l b i = do
  conf <- readMaybe <$> readFile
  if isNothing conf then errorLoad else $ do
    let BoardConf { confLayer = (layer,instr)
                  , confTempo = tempo
                  , confBoard = board
                  } = fromJust conf
    reactiveValueWrite t tempo
    reactiveValueWrite l layer
    reactiveValueWrite b $ boardInit board
    reactiveValueWrite i instr

errorLoad :: IO ()
errorLoad = undefined
