module RMCA.Translator.Controller where

import RMCA.Translator.Message

data Controller = Lol

messageToController :: Message -> Controller
messageToController _ = Lol

controllerToMessages :: Controller -> Message
controllerToMessages = undefined
