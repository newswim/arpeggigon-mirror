#!/bin/bash

mkdir -p ~/.log

jackd -d alsa --device hw:0 --rate 44100 --period 1024 &> ~/.log/jack.log &

sleep 5

qjackctl &

fluidsynth -a jack -m jack /usr/share/soundfonts/FluidR3_GM2-2.sf2 \
    &> ~/.log/fluidsynth.log
