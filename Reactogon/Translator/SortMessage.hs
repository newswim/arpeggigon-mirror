-- The idea is that the stream of data coming from the MIDI input port
-- will be sorted in three categories: note on events, controller
-- events and other events. The latter will be transmitted as is
-- through the whole systems.

module Reactogon.Message.SortMessage where

import Reactogon.Semantics

sortMessage :: [Message] -> ([Note], [Command], [RawMessage])
sortMessage = undefined
