{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}

-- Contains all the information and functions necessary to run a Jack
-- port and exchange information through reactive values and Yampa.
module RMCA.Translator.Jack ( jackSetup
                            ) where

import           Control.Arrow
import qualified Control.Monad.Exception.Synchronous as Sync
import qualified Control.Monad.Trans.Class           as Trans
import           Data.CBRef
import           Data.Foldable                       hiding (concat, mapM_)
import qualified Data.IntMap                         as M
import           Data.Maybe
import           Data.ReactiveValue
import qualified Foreign.C.Error                     as E
import           Graphics.UI.Gtk
import           RMCA.IOClockworks
import           RMCA.Layer.LayerConf
import           RMCA.ReactiveValueAtomicUpdate
import           RMCA.Semantics
import           RMCA.Translator.Message
import           RMCA.Translator.RV
import           RMCA.Translator.Translator
import qualified Sound.JACK                          as Jack
import qualified Sound.JACK.Exception                as JackExc
import qualified Sound.JACK.MIDI                     as JMIDI

rmcaName :: String
rmcaName = "arpeggigon"

inPortName :: String
inPortName = "input"

outPortName :: String
outPortName = "output"

handleErrorJack :: JackExc.All -> IO ()
handleErrorJack _ = postGUIAsync $ do
  diag <- messageDialogNewWithMarkup
          Nothing [] MessageError ButtonsClose
          "No running instance of Jack could be found!"
  widgetShow diag
  resp <- dialogRun diag
  mainQuit

-- Starts a default client with an input and an output port. Doesn't
-- do anything as such.
jackSetup :: (ReactiveValueAtomicUpdate board
              (M.IntMap ([Note],[Message])) IO
             , ReactiveValueRead tempo Tempo IO
             , ReactiveValueAtomicUpdate layerConfs (M.IntMap LayerConf) IO
             ) =>
             IOTick
          -> board
          -> tempo
          -> layerConfs
          -> IO ()
jackSetup tc boardQueue tempoRV layerMapRV = Sync.resolveT handleErrorJack $ do
  toProcessRV <- Trans.lift $ newCBRef []
  Jack.withClientDefault rmcaName $ \client -> do
    sr <- Trans.lift $ Jack.getSampleRate client
    Jack.withPort client outPortName $ \output ->
      Jack.withPort client inPortName  $ \input ->
      Jack.withProcess client (jackCallBack tc sr input output
                               toProcessRV boardQueue tempoRV layerMapRV) $
      Jack.withActivation client $ Trans.lift $ do
      putStrLn $ "Started " ++ rmcaName ++ " JACK client."
      Jack.waitForBreak
      return ()

-- The callback function. It pumps value out of the input port, mix
-- them with value coming from the machine itself and stuff them into
-- the output port. When this function is not running, events are
-- processed.
jackCallBack :: ( ReactiveValueAtomicUpdate toProcess [(Frames, RawMessage)] IO
                , ReactiveValueAtomicUpdate board
                  (M.IntMap ([Note],[Message])) IO
                , ReactiveValueRead tempo Tempo IO
                , ReactiveValueAtomicUpdate layerConfs (M.IntMap LayerConf) IO
                ) =>
                IOTick
             -> SampleRate
             -> JMIDI.Port Jack.Input
             -> JMIDI.Port Jack.Output
             -> toProcess
             -> board
             -> tempo
             -> layerConfs
             -> Jack.NFrames
             -> Sync.ExceptionalT E.Errno IO ()
jackCallBack tc sr input output toProcessRV boardQueue tempoRV layerMapRV
  nframes@(Jack.NFrames nframesInt') = do
  let inMIDIRV = inMIDIEvent input nframes
      outMIDIRV = outMIDIEvent output nframes
      nframesInt = fromIntegral nframesInt' :: Int
  Trans.lift $ do
    tempo <- reactiveValueRead tempoRV
    inMIDI <- reactiveValueRead inMIDIRV
    let (unchangedMessages,toBeTreatedMessages) =
          break (\(_,m) -> fromMaybe False $ do
                    mess <- fromRawMessage m
                    return (isInstrument mess || isVolume mess)) inMIDI
    reactiveValueAppend toProcessRV unchangedMessages
    let (volume,instruments) = break (isInstrument . snd) $
          map (second (fromJust . fromRawMessage)) toBeTreatedMessages
    mapM_ ((\(Volume c v) -> reactiveValueUpdate layerMapRV
            (M.adjust (\(st,d,s) -> (st,d,s { volume = v }))
             (fromChannel c))) . snd) volume
    mapM_ ((\(Instrument c p) -> reactiveValueUpdate layerMapRV
            (M.adjust (\(st,d,s) -> (st,d,s { instrument = fromProgram p }))
              (fromChannel c))) . snd) instruments
    toProcess <- reactiveValueRead toProcessRV
    {-fmap (concat . toList . gatherMessages sr tempo)
         (reactiveValueEmpty boardQueue) >>=
      reactiveValueAppend toProcessRV-}
    fmap ((`removeRepeat` toProcess) . concat . toList . getMessages sr tempo)
         (reactiveValueEmpty boardQueue) >>=
      reactiveValueWrite toProcessRV
    (go, old') <- fmap (schedule nframesInt) (reactiveValueRead toProcessRV)
    {-if not $ null $ map (second fromRawMessage) go then
      do print $ toShow $ map (second fromRawMessage) go
    else
      do putChar ' '-}
    let old = map (first (+ (- nframesInt))) old'
    reactiveValueWrite outMIDIRV go
    reactiveValueWrite toProcessRV old
    tickIOTick tc
  --------------

toShow :: [(Frames, Maybe Message)] -> String
toShow as = case as of
          [] -> []
          (v, Just n@(NoteOn _ p _)) : xs -> show p++"-NoteOn "++toShow xs
          (v, Just n@(NoteOff _ p _)) : xs -> show p++"-NoteOff "++toShow xs
          x : xs -> toShow xs

fromRaws :: [(Frames, RawMessage)] -> [(Frames, Message)]
fromRaws = fst . sortRawMessages

toRaws :: [(Frames, Message)] -> [(Frames, RawMessage)]
toRaws = map (second toRawMessage)

removeRepeat :: [(Frames, Message)] -> [(Frames, RawMessage)] -> [(Frames, RawMessage)]
removeRepeat fms frs = toRaws $ checkAndInsert fms (fromRaws frs)
    where checkAndInsert :: [(Frames, Message)] -> [(Frames, Message)] -> [(Frames, Message)]
          checkAndInsert fms [] = fms
          checkAndInsert fms (fm@(_, m) : fromRaws) | isNoteOff m = searchNoteOn fms fm : checkAndInsert fms fromRaws
                                                    | otherwise   = fm : checkAndInsert fms fromRaws

searchNoteOn :: [(Frames, Message)] -> (Frames, Message) -> (Frames, Message)
searchNoteOn [] fm = fm
searchNoteOn ((frames, NoteOn _ p' _) : fms) fm@(_, m@(NoteOff _ p _)) = case p' == p of
                                                    True -> (frames, m)
                                                    False -> searchNoteOn fms fm
searchNoteOn (fm' : fms) fm = searchNoteOn fms fm