{-# LANGUAGE Arrows #-}

module RMCA.Layer.Board ( boardRun
                        , BoardRun (..)
                        ) where

import qualified Data.IntMap       as M
import           Data.List         ((\\))
import           FRP.Yampa
import           RMCA.Auxiliary
import           RMCA.Global.Clock
import           RMCA.Layer.Layer
import           RMCA.Semantics

data BoardRun = BoardStart | BoardStop deriving (Eq, Show)

singleBoard :: [PlayHead]
            -> SF (Board, Layer, Event BeatNo) (Event ([PlayHead], [Note]))
singleBoard iPh = proc (board, Layer { relPitch = rp
                                     , strength = s
                                     }, ebno) ->
  accumBy advanceHeads' (iPh,[]) -< ebno `tag` (board, fromEvent ebno, rp, s)
  where advanceHeads' (ph,_) (board,bno,rp,s) = advanceHeads board bno rp s ph

boardSF :: SF (Event AbsBeat, Board, Layer, BoardRun)
              (Event ([PlayHead], [Note]))
boardSF = proc (eabs, board, l, br) -> do
  ebno <- layerMetronome -< (eabs,l)
  ess <- onChange -< br
  boardSwitch [] -< ((board, l, ebno), ess `tag` (br, startHeads board))

boardSwitch :: [PlayHead]
            -> SF ((Board, Layer, Event BeatNo), Event (BoardRun, [PlayHead]))
                  (Event ([PlayHead],[Note]))
boardSwitch rPh = dSwitch (singleBoard rPh *** (identity >>> notYet)) fnSwitch
  where fnSwitch (BoardStart, iPh) = boardSwitch iPh
        fnSwitch (BoardStop, _) = boardSwitch []

--------------------------------------------------------------------------------
-- Machinery to make boards run in parallel
--------------------------------------------------------------------------------

boardRun' :: M.IntMap (SF (Event AbsBeat,Board,Layer,BoardRun)
                          (Event ([PlayHead],[Note])))
          -> SF (Event AbsBeat, BoardRun, M.IntMap (Board,Layer))
                (M.IntMap (Event ([PlayHead],[Note])))
boardRun' iSF = boardRun'' iSF (lengthChange iSF)
  where boardRun'' iSF swSF = pSwitch routeBoard iSF swSF contSwitch
        contSwitch contSig (oldSig, newSig) = boardRun'' newSF
                                              (lengthChange newSF >>> notYet)
          where newSF = foldr (\k m -> M.insert k boardSF m)
                        (foldr M.delete contSig oldSig) newSig
        lengthChange iSig = edgeBy diffSig ik <<^ M.keys <<^ (\(_,_,x) -> x) <<^ fst
          where ik = M.keys iSig
          -- Old elements removed in nL are on the left, new elements added to
          -- nL are on the right.
                diffSig :: [Int] -> [Int] -> Maybe ([Int],[Int])
                diffSig oL nL
                  | oL == nL = Nothing
                  | otherwise = Just (oL \\ nL, nL \\ oL)
        routeBoard :: (Event AbsBeat,BoardRun,M.IntMap (Board,Layer))
                   -> M.IntMap sf
                   -> M.IntMap ((Event AbsBeat,Board,Layer,BoardRun),sf)
        routeBoard (evs,br,map) =
          M.intersectionWith (,) ((\(b,l) -> (evs,b,l,br)) <$> map)

boardRun :: (Tempo, BoardRun, M.IntMap (Board,Layer))
         -> SF (Tempo, BoardRun, M.IntMap (Board,Layer))
               (M.IntMap (Event ([PlayHead],[Note])))
boardRun (_,_,iMap) = mkBeat >>> boardRun' (iMap $> boardSF)
  where mkBeat = proc (t,x,y) -> do
          e <- metronome -< t
          returnA -< (e,x,y)
