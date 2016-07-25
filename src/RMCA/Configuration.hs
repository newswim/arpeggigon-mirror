{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables #-}

module RMCA.Configuration where

import           Control.Exception
import           Data.Array
import qualified Data.Bifunctor     as BF
import           Data.Maybe
import           Data.ReactiveValue
import           Graphics.UI.Gtk
import           RMCA.Auxiliary
import           RMCA.GUI.Board
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
  catch (writeFile fp $ show bc) (\(_ :: IOError) -> errorSave)

loadConfiguration :: ( ReactiveValueWrite tempo Tempo IO
                     , ReactiveValueWrite layer Layer IO
                     , ReactiveValueWrite cell GUICell IO
                     , ReactiveValueWrite instr InstrumentNo IO) =>
                     FilePath -> tempo -> layer
                  -> Array Pos cell -> instr -> IO ()
loadConfiguration fp t l arr i = do
  conf <- readMaybe <$> readFile fp
  if isNothing conf then errorLoad else
    do let BoardConf { confLayer = (layer,instr)
                     , confTempo = tempo
                     , confBoard = (BoardInit board)
                     } = fromJust conf
       reactiveValueWrite t tempo
       reactiveValueWrite l layer
       mapM_ (\rv -> catch (reactiveValueWrite rv inertCell)
                     (\(_ :: ErrorCall) -> return ())) $ elems arr
       mapM_ (\(p,(a,r)) -> reactiveValueWrite (arr ! toGUICoords p) $
                            inertCell { cellAction = a
                                      , repeatCount = r
                                      }) board
       reactiveValueWrite i instr

errorLoad :: IO ()
errorLoad =  messageDialogNewWithMarkup Nothing [] MessageError ButtonsClose
             "Error loading the configuration file!" >>= widgetShow

errorSave :: IO ()
errorSave =  messageDialogNewWithMarkup Nothing [] MessageError ButtonsClose
             "Error saving the configuration file!" >>= widgetShow

handleSaveLoad :: ( ReactiveValueRead save () IO
                  , ReactiveValueRead load () IO
                  , ReactiveValueReadWrite instr InstrumentNo IO
                  , ReactiveValueReadWrite layer Layer IO
                  , ReactiveValueRead board Board IO
                  , ReactiveValueReadWrite tempo Tempo IO
                  , ReactiveValueWrite cell GUICell IO) =>
                  tempo -> board -> layer -> instr
               -> Array Pos cell -> save -> load -> IO ()
handleSaveLoad tempoRV boardRV layerRV instrRV pieceArrRV confSaveRV confLoadRV = do
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
          fmap (\f -> loadConfiguration f tempoRV layerRV pieceArrRV instrRV)
        respHandle _ = return ()

    onResponse fcl (\r -> respHandle r >> widgetHide fcl)
    return ()
