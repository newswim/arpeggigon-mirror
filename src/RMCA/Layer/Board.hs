{-# LANGUAGE Arrows #-}

module RMCA.Layer.Board ( boardRun
                        , BoardRun (..)
                        ) where

import qualified Data.IntMap      as M
import           Data.List        ((\\))
import           FRP.Yampa
import           RMCA.Auxiliary
import           RMCA.Layer.Layer
import           RMCA.Semantics

data BoardRun = BoardStart | BoardStop deriving (Eq, Show)

singleBoard :: [PlayHead]
            -> SF (Board, Layer, Event BeatNo) (Event ([PlayHead], [Note]))
singleBoard iPh = proc (board, Layer { relPitch = rp
                                     , strength = s
                                     }, ebno) ->
  accumBy advanceHeads' (iPh,[]) -< ebno `tag` (board, fromEvent ebno, rp, s)
  where advanceHeads' (ph,_) (board,bno,rp,s) = uncurry5 advanceHeads (board,bno,rp,s,ph)

boardSF :: SF (Board, Layer, Tempo, BoardRun) (Event ([PlayHead], [Note]))
boardSF = proc (board, l, t, br) -> do
  ebno <- layerMetronome -< (t,l)
  ess <- onChange -< br
  boardSwitch [] -< ((board, l, ebno), ess `tag` (br, startHeads board))

boardSwitch :: [PlayHead]
            -> SF ((Board, Layer,Event BeatNo), Event (BoardRun, [PlayHead]))
               (Event ([PlayHead],[Note]))
boardSwitch rPh = dSwitch (singleBoard rPh *** (identity >>> notYet)) fnSwitch
  where fnSwitch (BoardStart, iPh) = boardSwitch iPh
        fnSwitch (BoardStop, _) = boardSwitch []

--------------------------------------------------------------------------------
-- Machinery to make parallel boards run
--------------------------------------------------------------------------------

routeBoard :: M.IntMap a -> M.IntMap sf -> M.IntMap (a,sf)
routeBoard = M.intersectionWith (,)

-- On the left are the disappearing signals, on the right the
-- appearing one.
lengthChange :: M.IntMap b -> SF (M.IntMap a, M.IntMap sf) (Event ([Int],[Int]))
lengthChange iSig = edgeBy diffSig ik <<^ M.keys <<^ fst
  where ik = M.keys iSig
        -- Old elements removed in nL are on the left, new elements added to
        -- nL are on the right.
        diffSig :: [Int] -> [Int] -> Maybe ([Int],[Int])
        diffSig oL nL
          | oL == nL = Nothing
          | otherwise = Just (oL \\ nL, nL \\ oL)

boardRun' :: M.IntMap (SF (Board,Layer,Tempo,BoardRun)
                          (Event ([PlayHead],[Note])))
          -> SF (M.IntMap (Board,Layer,Tempo,BoardRun))
                (M.IntMap (Event ([PlayHead],[Note])))
boardRun' iSF = boardRun'' iSF (lengthChange iSF)
  where boardRun'' iSF swSF = pSwitch routeBoard iSF swSF contSwitch
        contSwitch contSig (oldSig, newSig) = boardRun'' newSF
                                              (lengthChange newSF >>> notYet)
          where newSF = foldr (\k m -> M.insert k boardSF m)
                        (foldr M.delete contSig oldSig) newSig

boardRun :: M.IntMap (Board,Layer,Tempo,BoardRun)
         -> SF (M.IntMap (Board,Layer,Tempo,BoardRun))
               (M.IntMap (Event ([PlayHead],[Note])))
boardRun iSig = boardRun' (iSig $> boardSF)
