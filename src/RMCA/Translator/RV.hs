{-# LANGUAGE ScopedTypeVariables #-}

module RMCA.Translator.RV where

import           Control.Monad.Exception.Synchronous (ExceptionalT, resolveT)
import           Data.Monoid                         hiding (All)
import qualified Data.Bifunctor                      as BF
import qualified Data.EventList.Absolute.TimeBody    as EventListAbs
import qualified Data.List                           as L
import           Data.Ord                            (comparing)
import           Data.ReactiveValue
import           RMCA.Translator.Message
import qualified Sound.JACK                          as Jack
import           Sound.JACK.Exception                (All, toStringWithHead)
import qualified Sound.JACK.MIDI                     as JMIDI
import qualified System.IO                           as IO

handleError :: (Monoid a) => ExceptionalT All IO a -> IO a
handleError = resolveT $ \e -> do
  IO.hPutStrLn IO.stderr $ toStringWithHead e
  return mempty

inMIDIEvent :: JMIDI.Port Jack.Input
            -> Jack.NFrames
            -> ReactiveFieldRead IO [(Frames,RawMessage)]
inMIDIEvent input nframes = ReactiveFieldRead getter notifier
  where getter :: IO [(Frames, RawMessage)]
        getter = handleError $ 
                     fmap transform (JMIDI.readEventsFromPort input nframes)

        transform :: EventListAbs.T Jack.NFrames t -> [(Frames, t)]
        transform = map (BF.first (\(Jack.NFrames n) -> fromIntegral n)) .
                    EventListAbs.toPairList

        notifier :: IO () -> IO ()
        notifier = id

outMIDIEvent :: JMIDI.Port Jack.Output
             -> Jack.NFrames
             -> ReactiveFieldWrite IO [(Frames, RawMessage)]
outMIDIEvent output nframes@(Jack.NFrames nframesInt') =
  ReactiveFieldWrite setter
  where setter :: [(Frames, RawMessage)] -> IO ()
        setter = handleError .
                 JMIDI.writeEventsToPort output nframes . transform
        -- Doesn't assume the list is sorted or small enough. For
        -- large size buffer, this might cause performance issue. All
        -- the unprocessed events are lost, which is unfortunate…
        transform :: [(Frames, RawMessage)]
                  -> EventListAbs.T Jack.NFrames RawMessage
        transform = EventListAbs.fromPairList .
                    map (BF.first (Jack.NFrames . fromIntegral)) .
                    takeWhile ((< nframesInt) . fst) . L.sortBy (comparing fst)
        nframesInt = fromIntegral nframesInt'
