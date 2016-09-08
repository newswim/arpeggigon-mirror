{-# LANGUAGE Arrows #-}

module RMCA.Layer.Board ( boardRun
                        , SwitchBoard (..)
                        ) where

import qualified Data.IntMap          as M
import           Data.List            ((\\))
import           FRP.Yampa
import           RMCA.Global.Clock
import           RMCA.Layer.LayerConf
import           RMCA.Semantics

data SwitchBoard = StartBoard StaticLayerConf
                 | ContinueBoard
                 | StopBoard

updatePhOnSwitch :: Board -> [PlayHead] -> SwitchBoard -> [PlayHead]
updatePhOnSwitch _ _ (StopBoard {}) = []
updatePhOnSwitch board _ (StartBoard {}) = startHeads board
updatePhOnSwitch board oldPhs (ContinueBoard {}) = oldPhs ++ startHeads board
{-
noStopBoard :: Event SwitchBoard -> Event SwitchBoard
noStopBoard (Event (StopBoard {})) = NoEvent
noStopBoard e = e
-}
{-
genPlayHeads :: Board -> SwitchBoard -> [PlayHead]
genPlayHeads _ (StopBoard {}) = []
genPlayHeads board _ = startHeads board
-}
{-
continueBoard :: Event SwitchBoard -> Event [PlayHead]
continueBoard board (Event (ContinueBoard {})) = Event $ startHeads board
continueBoard _ _ = NoEvent
-}
startBoard :: Event SwitchBoard -> Event StaticLayerConf
startBoard (Event (StartBoard st)) = Event st
startBoard _ = NoEvent

stopBoard :: Event SwitchBoard -> Event SwitchBoard
stopBoard e@(Event StopBoard) = e
stopBoard _ = NoEvent

-- singleboard is a simple running board. Given an initial list of
-- play heads, it runs the board by the beat. It produces events but
-- also a constant output of the states of the play heads to allow for
-- adding them.
singleBoard :: [PlayHead]
            -> SF (Board,DynLayerConf,Event BeatNo)
                  (Event [Note], [PlayHead])
singleBoard iPh = proc (board, DynLayerConf { relPitch = rp
                                            , strength = s
                                            }, ebno) -> do
  (phs,notes) <- accumHoldBy advanceHeads' (iPh,[])
                 -< ebno `tag` (board, fromEvent ebno, rp, s)
  returnA -< (ebno `tag` notes, phs)
  where advanceHeads' (ph,_) (board,bno,rp,s) = advanceHeads board bno rp s ph

-- dynSingleBoard differs from singleBoard in that it receives a
-- SwitchBoard event allowing it to start/stop the board.
dynSingleBoard :: SF (Board, DynLayerConf, Event BeatNo, Event SwitchBoard)
                  (Event [Note], [PlayHead])
dynSingleBoard = proc (board, dynConf, ebno, esb) -> do
  rec
    res@(_,curPhs) <- rSwitch $ singleBoard []
      -< ( (board, dynConf, ebno)
         , fmap (singleBoard . updatePhOnSwitch board curPhs') esb)
    curPhs' <- iPre [] -< curPhs
  returnA -< res

boardSF :: StaticLayerConf
        -> SF (Event AbsBeat, Board, DynLayerConf, Event SwitchBoard)
              (Event [Note], [PlayHead])
boardSF (StaticLayerConf { beatsPerBar = bpb }) =
  proc (eabs, board, dynConf, esb) -> do
    ebno <- rSwitch never -< ( (eabs,dynConf)
                             , layerMetronome <$> startBoard esb)
    dynSingleBoard -< (board,dynConf,ebno,esb)

----------------------------------------------------------------------------
-- Machinery to make boards run in parallel
----------------------------------------------------------------------------

boardRun' :: M.IntMap (SF (Event AbsBeat,Board,DynLayerConf,Event SwitchBoard)
                          (Event [Note], [PlayHead]))
          -> SF (Event AbsBeat, M.IntMap (Board,DynLayerConf,Event SwitchBoard))
                (M.IntMap (Event [Note], [PlayHead]))
boardRun' iSF = boardRun'' iSF (lengthChange iSF)
  where boardRun'' iSF swSF = pSwitch routeBoard iSF swSF contSwitch
        contSwitch contSig (oldSig, newSig) = boardRun'' newSF
                                              (lengthChange newSF >>> notYet)
          where defaultBoardSF = boardSF defaultStaticLayerConf
                newSF = foldr (\k m -> M.insert k defaultBoardSF m)
                        (foldr M.delete contSig oldSig) newSig
        lengthChange iSig = edgeBy diffSig ik <<^ M.keys <<^ (\(_,x) -> x) <<^ fst
          where ik = M.keys iSig
          -- Old elements removed in nL are on the left, new elements added to
          -- nL are on the right.
                diffSig :: [Int] -> [Int] -> Maybe ([Int],[Int])
                diffSig oL nL
                  | oL == nL = Nothing
                  | otherwise = Just (oL \\ nL, nL \\ oL)
        routeBoard :: (Event AbsBeat,M.IntMap (Board,DynLayerConf,Event SwitchBoard))
                   -> M.IntMap sf
                   -> M.IntMap ((Event AbsBeat,Board,DynLayerConf,Event SwitchBoard),sf)
        routeBoard (evs,map) sfs =
          M.intersectionWith (,) ((\(b,l,ebs) -> (evs,b,l,ebs)) <$> map) sfs

boardRun :: M.IntMap StaticLayerConf
         -> SF (Tempo, M.IntMap (Board,DynLayerConf,Event SwitchBoard))
               (M.IntMap (Event [Note], [PlayHead]))
boardRun iMap = mkBeat >>> (boardRun' $ fmap boardSF iMap)
  where mkBeat = proc (t,map) -> do
          esb <- arr (foldr selEvent NoEvent) <<^ fmap (\(_,_,e) -> e) -< map
          eab <- rSwitch never -< (t, lMerge (stopBoard esb `tag` never)
                                      (startBoard esb `tag` metronome))
          returnA -< (eab,map)
        selEvent x NoEvent = x
        selEvent e@(Event (StopBoard {})) _ = e
        selEvent (Event (StartBoard {})) f@(Event (StopBoard {})) = f
        selEvent _ x = x
