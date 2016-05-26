{-# LANGUAGE Arrows #-}

module Reactogon.Translator.Translator where

import Reactogon.Semantics
import Reactogon.Translator.Message

-- Takes a stream of raw messages and translates them by type.
fromRaw :: SF RawMessage (Note, SystemMessage, RawMessage)
fromRaw = undefined

-- Takes a stream of high level messages and translates them by type.
toRaw :: SF (Note, SystemMessage, RawMessage) RawMessage
toRaw = undefined
