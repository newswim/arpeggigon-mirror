{-# LANGUAGE Arrows #-}

module Reactogon.Translator.Translator where

import FRP.Yampa
import Reactogon.Semantics
import Reactogon.Translator.Message
import Reactogon.Translator.SortMessage

-- Takes a stream of raw messages and translates them by type.
fromRaw :: SF RawMessage (Note, Controller, RawMessage)
fromRaw = undefined

-- Takes a stream of high level messages and translates them by type.
toRaw :: SF (Note, Controller, RawMessage) RawMessage
toRaw = undefined
