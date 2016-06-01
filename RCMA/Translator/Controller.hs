module RCMA.Translator.Controller where

import RCMA.Semantics
import RCMA.Translator.Message

messageToController :: Message -> Controller
messageToController _ = Lol

controllerToMessages :: Controller -> Message
controllerToMessages = undefined
