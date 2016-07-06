-- Contains button name definition
module RMCA.GUI.Buttons where

import System.Glib

gtkMediaPlay :: DefaultGlibString
gtkMediaPlay = stringToGlib "gtk-media-play"

gtkMediaStop :: DefaultGlibString
gtkMediaStop = stringToGlib "gtk-media-stop"

gtkMediaPause :: DefaultGlibString
gtkMediaPause = stringToGlib "gtk-media-pause"

gtkMediaRecord :: DefaultGlibString
gtkMediaRecord = stringToGlib "gtk-media-record"
