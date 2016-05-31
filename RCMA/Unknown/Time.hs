module Time ( toFrames
            , fromFrames
            ) where

import FRP.Yampa
import Sound.JACK (NFrames(NFrames))

import MIDI

toFrames :: SampleRate -> DTime -> NFrames
toFrames s = NFrames . floor . (fromIntegral s *)

fromFrames :: SampleRate -> NFrames -> DTime
fromFrames s (NFrames n) = fromIntegral n/fromIntegral s
