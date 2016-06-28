module RMCA.Translator.Controller where

import RMCA.Semantics
import RMCA.Translator.Message

messageToController :: Message -> Controller
messageToController _ = Lol

controllerToMessages :: Controller -> Message
controllerToMessages = undefined
