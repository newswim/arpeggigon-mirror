{-# LANGUAGE Arrows #-}

module RMCA.Layer.Board where

import qualified Data.IntMap          as M
import           Data.List            ((\\))
import           FRP.Yampa
import           RMCA.Auxiliary
import           RMCA.Global.Clock
import           RMCA.Layer.LayerConf
import           RMCA.Semantics

import           Debug.Trace

data RunStatus = Running | Stopped

automaton :: [PlayHead]
          -> SF (Board, DynLayerConf, Event BeatNo)
                (Event [Note], [PlayHead])
automaton iphs = proc (b, DynLayerConf { relPitch = rp
                                       , strength = s
                                       }, ebno) -> do
  enphs     <- accumBy advanceHeads' (iphs,[])
                          -< ebno `tag` (b, fromEvent ebno, rp, s)
  (ephs,en) <- arr splitE -< enphs
  phs       <- hold iphs  -< ephs
  returnA                 -< (en, phs)
  where advanceHeads' (ph,_) (board,bno,rp,s) = advanceHeads board bno rp s ph


layer :: SF (Event AbsBeat, Board, LayerConf, Event RunStatus)
            (Event [Note], [PlayHead])
layer = layerStopped
  where switchStatus (rs, slc, iphs) = case rs of
          Stopped -> layerStopped
          Running -> layerRunning slc iphs

        layerStopped = switch lsAux switchStatus

        layerRunning slc iphs = switch (lrAux slc iphs) switchStatus

        lsAux = proc (_, b, (slc,_,_), ers) -> do
          en  <- never       -< ()
          phs <- constant [] -< ()
          e   <- notYet      -< fmap (\rs -> (rs, slc, startHeads b)) ers
          returnA            -< ((en,phs),e)

        lrAux slc iphs = proc (eab, b, (slc',dlc,_), ers) -> do
          ebno  <- layerMetronome slc -< (eab, dlc)
          enphs@(_,phs) <- automaton iphs -< (b, dlc, ebno)
          r <- (case let a = repeatCount slc in traceShow a a of
                  Nothing -> never
                  Just n -> countTo (n * beatsPerBar slc)) -< ebno
          let ers' = ers `lMerge` (r `tag` Running)
          e <- notYet -< fmap (\rs -> (rs, slc', phs ++ startHeads b)) ers'
          returnA -< (enphs,e)

layers :: M.IntMap a
       -> SF (Tempo, Event RunStatus,
              M.IntMap (Board,LayerConf,Event RunStatus))
             (M.IntMap (Event [Note], [PlayHead]))
layers imap = proc (t,erun,map) -> do
  elc <- edgeBy diffSig (M.keys imap) -< M.keys map
  let e = fmap switchCol elc
      newMetronome Running = metronome
      newMetronome Stopped = never
  eabs <- rSwitch metronome -< (t, newMetronome <$> erun)
  rpSwitch routing (imap $> layer) -< ((eabs,erun,map),e)
  where routing (eabs,erun,map) sfs = M.intersectionWith (,)
          (fmap (\(b,l,er) -> (eabs,b,l,erun `lMerge` er)) map) sfs

        diffSig :: [Int] -> [Int] -> Maybe ([Int],[Int])
        diffSig oL nL
          | oL == nL = Nothing
          | otherwise = Just (oL \\ nL, nL \\ oL)

        switchCol (oldSig,newSig) col =
          foldr (\k m -> M.insert k layer m)
          (foldr M.delete col oldSig) newSig
