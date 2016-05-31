{-# LANGUAGE Arrows #-}

module RCMA.Translator.Translator where

import FRP.Yampa
import RCMA.Semantics
import RCMA.Translator.Message
import RCMA.Translator.SortMessage

-- Takes a stream of raw messages and translates them by type.
fromRaw :: SF RawMessage (Note, Controller, RawMessage)
fromRaw = undefined

-- Takes a stream of high level messages and translates them by type.
toRaw :: SF (Note, Controller, RawMessage) RawMessage
toRaw = undefined
