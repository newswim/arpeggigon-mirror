{-# LANGUAGE Arrows #-}

-- Contains all the information and functions necessary to run a Jack
-- port and exchange information through reactive values and Yampa.
module Reactogon.Translator.Jack where

import qualified Control.Monad.Exception.Synchronous as Sync
import qualified Control.Monad.Trans.Class           as Trans
import qualified Foreign.C.Error                     as E
import           Hails.Yampa
import           Reactogon.Translator.Message
import qualified Sound.JACK                          as Jack
import qualified Sound.JACK.MIDI                     as JMIDI

reactogonName :: String
reactogonName = "Reactogon"

inPortName :: String
inPortName = "input"

outPortName :: String
outPortName = "output"

jackSetup :: IO ()
jackSetup = Jack.handleExceptions $
  Jack.withClientDefault reactogonName $ \client ->
  Jack.withPort client outPortName $ \output ->
  Jack.withPort client inPortName  $ \input ->
  jackRun client input output (jackCallBack client input output)

jackRun client input output callback =
  Jack.withProcess client callback $ do
  Trans.lift $ putStrLn $ "Started " ++ reactogonName
  Trans.lift $ Jack.waitForBreak

jackCallBack client input output = undefined
