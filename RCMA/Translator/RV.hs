{-# LANGUAGE ScopedTypeVariables #-}

module Reactogon.Translator.RV where

import           Control.Monad.Exception.Synchronous (ExceptionalT)
import qualified Data.Bifunctor                      as BF
import qualified Data.EventList.Absolute.TimeBody    as EventListAbs
import qualified Data.List                           as L
import           Data.Ord                            (comparing)
import           Data.ReactiveValue
import           RCMA.Translator.Message
import qualified Sound.JACK                          as Jack
import           Sound.JACK.Exception                (ThrowsErrno)
import qualified Sound.JACK.MIDI                     as JMIDI

readMIDIEvent :: forall e. (ThrowsErrno e) =>
                 JMIDI.Port Jack.Input
              -> Jack.NFrames
              -> ReactiveFieldRead (ExceptionalT e IO) [(Frames,RawMessage)]
readMIDIEvent input nframes = ReactiveFieldRead getter notifier
  where getter :: ExceptionalT e IO [(Frames, RawMessage)]
        getter = transform <$> (JMIDI.readEventsFromPort input nframes)

        transform :: EventListAbs.T Jack.NFrames t -> [(Frames, t)]
        transform = map (BF.first (\(Jack.NFrames n) -> fromIntegral n)) .
                    EventListAbs.toPairList

        notifier :: ExceptionalT e IO () -> ExceptionalT e IO ()
        notifier _ = return ()

writeMIDIEvent :: forall e. (ThrowsErrno e) =>
                  JMIDI.Port Jack.Output
               -> Jack.NFrames
               -> ReactiveFieldWrite (ExceptionalT e IO) [(Frames, RawMessage)]
writeMIDIEvent input nframes@(Jack.NFrames nframesInt') =
  ReactiveFieldWrite setter
  where setter :: [(Frames, RawMessage)] -> ExceptionalT e IO ()
        setter = JMIDI.writeEventsToPort input nframes . transform
        -- Doesn't assume the list is sorted or small enough. For
        -- large size buffer, this might cause performance issue. All
        -- the unprocessed events are lost, which is unfortunate…
        transform :: [(Frames, RawMessage)]
                  -> EventListAbs.T Jack.NFrames RawMessage
        transform = EventListAbs.fromPairList .
                    map (BF.first (Jack.NFrames . fromIntegral)) .
                    takeWhile ((< nframesInt) . fst) . L.sortBy (comparing fst)
        nframesInt = fromIntegral nframesInt'
