{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, PartialTypeSignatures,
             ScopedTypeVariables #-}

module RMCA.Configuration where

import           Control.Arrow
import           Control.Exception
import           Data.Array
import qualified Data.IntMap         as M
import           Data.List
import           Data.Maybe
import           Data.ReactiveValue
import           Graphics.UI.Gtk
import           RMCA.Auxiliary
import           RMCA.GUI.Board
import           RMCA.GUI.MultiBoard
import           RMCA.Layer.Layer
import           RMCA.Semantics
import           Text.Read

type InstrumentNo = Int

data BoardConf = BoardConf { confLayers :: [(BoardInit,Layer,InstrumentNo)]
                           , confTempo  :: Tempo
                           } deriving(Read,Show)

newtype BoardInit = BoardInit { toList :: [(Pos,Cell)] } deriving(Show,Read)

mkInit :: Board -> BoardInit
mkInit = BoardInit . filter (uncurry (&&) . (onBoard *** notDef)) . assocs
  where notDef (Inert,1) = False
        notDef _ = True

boardInit :: BoardInit -> Board
boardInit = makeBoard . toList

saveConfiguration :: ( ReactiveValueRead tempo Tempo IO
                     , ReactiveValueRead layer (M.IntMap Layer) IO
                     , ReactiveValueRead board (M.IntMap Board) IO
                     , ReactiveValueRead instr (M.IntMap InstrumentNo) IO) =>
                     FilePath -> tempo -> layer -> board -> instr -> IO ()
saveConfiguration fp t l b i = do
  tempo  <- reactiveValueRead t
  layers <- M.elems <$> reactiveValueRead l
  boards <- map mkInit <$> M.elems <$> reactiveValueRead b
  instrs <- M.elems <$> reactiveValueRead i
  let bc = BoardConf { confLayers = zip3 boards layers instrs
                     , confTempo = tempo
                     }
  catch (writeFile fp $ show bc) (\(_ :: IOError) -> errorSave)

-- Current solution to delete all existing layers is to write to the
-- rm button, which is not that nice.
loadConfiguration :: ( ReactiveValueWrite tempo Tempo IO
                     , ReactiveValueWrite layer (M.IntMap Layer) IO
                     , ReactiveValueWrite cell GUICell IO
                     , ReactiveValueWrite instr (M.IntMap InstrumentNo) IO
                     , ReactiveValueWrite addLayer () IO
                     , ReactiveValueWrite rmLayer () IO
                     , ReactiveValueRead boards (M.IntMap (Array Pos cell)) IO) =>
                     FilePath -> tempo -> layer
                  -> boards -> instr -> addLayer -> rmLayer -> IO ()
loadConfiguration fp t l arrs i addLayer rmLayer = do
  conf <- readMaybe <$> readFile fp
  if isNothing conf then errorLoad else
    do let BoardConf { confLayers = cl
                     , confTempo = tempo
                     } = fromJust conf
           (boards,layers,instrs) = unzip3 cl
           layNum = length cl
       sequence_ $ replicate maxLayers $ reactiveValueWrite rmLayer ()
       sequence_ $ replicate layNum $ reactiveValueWrite addLayer ()
       reactiveValueWrite t tempo
       reactiveValueWrite l $ M.fromList $ zip [1..] layers
       reactiveValueWrite i $ M.fromList $ zip [1..] instrs
       cellArrs <- reactiveValueRead arrs
       mapM_ (\(arr,board) ->
                do mapM_ (\rv -> catch (reactiveValueWrite rv inertCell)
                           (\(_ :: ErrorCall) -> return ())) $ elems arr
                   mapM_ (\(p,(a,r)) -> reactiveValueWrite (arr ! toGUICoords p) $
                                        inertCell { cellAction = a
                                                  , repeatCount = r
                                                  }) board
             ) $ M.intersectionWith (,) cellArrs
         $ M.fromList $ zip [1..] $ map (\(BoardInit b) -> b) boards

errorLoad :: IO ()
errorLoad =  messageDialogNewWithMarkup Nothing [] MessageError ButtonsClose
             "Error loading the configuration file!" >>= widgetShow

errorSave :: IO ()
errorSave =  messageDialogNewWithMarkup Nothing [] MessageError ButtonsClose
             "Error saving the configuration file!" >>= widgetShow

handleSaveLoad :: ( ReactiveValueReadWrite tempo Tempo IO
                  , ReactiveValueReadWrite layer (M.IntMap Layer) IO
                  , ReactiveValueWrite cell GUICell IO
                  , ReactiveValueReadWrite instr (M.IntMap InstrumentNo) IO
                  , ReactiveValueWrite addLayer () IO
                  , ReactiveValueWrite rmLayer () IO
                  , ReactiveValueRead boards (M.IntMap (Array Pos cell)) IO
                  , ReactiveValueRead load () IO
                  , ReactiveValueRead save () IO
                  , ReactiveValueRead board (M.IntMap Board) IO) =>
                  tempo -> board -> layer -> instr
               -> boards -> addLayer -> rmLayer -> save -> load -> IO ()
--handleSaveLoad :: _
handleSaveLoad tempoRV boardRV layerRV instrRV pieceArrRV addLayerRV rmLayerRV confSaveRV confLoadRV = do
  fcs <- fileChooserDialogNew (Just "Save configuration") Nothing
         FileChooserActionSave [("Cancel",ResponseCancel),("Ok",ResponseOk)]
  reactFilt <- fileFilterNew
  fileFilterAddPattern reactFilt "*.react"
  fileFilterSetName reactFilt "RMCA conf files."
  fileChooserAddFilter fcs reactFilt

  fcl <- fileChooserDialogNew (Just "Load configuration") Nothing
         FileChooserActionOpen [("Cancel",ResponseCancel),("Ok",ResponseOk)]
  fileChooserAddFilter fcl reactFilt

  reactiveValueOnCanRead confSaveRV $ postGUIAsync $ do
    widgetShowAll fcs
    let respHandle ResponseOk =
          fileChooserGetFilename fcs >>= fromMaybeM_ .
          fmap (\f -> saveConfiguration f tempoRV layerRV boardRV instrRV)
        respHandle _ = return ()

    onResponse fcs (\r -> respHandle r >> widgetHide fcs)
    return ()

  reactiveValueOnCanRead confLoadRV $ postGUIAsync $ do
    widgetShowAll fcl
    let respHandle ResponseOk =
          fileChooserGetFilename fcl >>= fromMaybeM_ .
          fmap (\f -> loadConfiguration f tempoRV layerRV pieceArrRV instrRV
                      addLayerRV rmLayerRV )
        respHandle _ = return ()

    onResponse fcl (\r -> respHandle r >> widgetHide fcl)

    return ()
