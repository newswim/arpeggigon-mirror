Arpeggigon
===

The arpeggigon is a music instrument inspired by Mark
Burton's [reacTogon][].

It is written in Haskell using the FRP
library [Yampa](http://hackage.haskell.org/package/Yampa-0.10.4) for
the music
generation, [Gtk2Hs](https://hackage.haskell.org/package/gtk-0.14.5)
for the Graphical User Interface (GUI)
and [jack](http://hackage.haskell.org/package/jack-0.7.1) for handling
MIDI I/O. The code to interface with the exterior world is structured
using reactive values and relations
(see
[this paper](http://www.cs.nott.ac.uk/~psxip1/papers/2015-HaskellSymposium-Perez-Nilsson-BridingGUIGapReactiveValues.pdf) and
the
libraries
[here](https://hackage.haskell.org/package/keera-hails-reactivevalues-0.2.2.0) and
[here](https://hackage.haskell.org/package/keera-hails-reactive-gtk-0.3)).

Install guide
---

The arpeggigon needs the [GTK+ 2.0 C API](http://www.gtk.org/) and
the [JACK C API](http://jackaudio.org/) to be built. It is known to
work with GHC 7.10.3 and GHC 8.0.1.

Running the arpeggigon is then possible with:

	cabal sandbox init
	cabal install --only-dependencies
    cabal run arpeggigon

To install the arpeggigon globally, one can call:

	cabal install

This will create an executable located in
`<path_to_repo>/.cabal-sandbox/bin/` from where it is possible to create a
symbolic link to an object in the `PATH`.

	ln -s <path_to_repo>/.cabal-sandbox/bin/arpeggigon <somewhere_in_PATH>/arpeggigon

Description
---

### The reacTogon

The [reacTogon][] is a musical instrument invented based upon
the
[harmonic table](https://en.wikipedia.org/wiki/Harmonic_table_note_layout),
a way of arranging notes where the various directions correspond to
meaningful musical intervals. From that layout, the reacTogon defines
an automaton: tokens of different kinds interact with *play heads*
moving around on the board, altering their directions and eventually
playing a note.

The arpeggigon is an extended reacTogon. It is for instance possible
to have multiple layers with different characteristics running in
parallel, or to modify the way individual notes should be played, etc.

### Music generation

The arpeggigon doesn't directly produce music, but is
a
[MIDI](https://en.wikipedia.org/wiki/MIDI) [JACK](http://jackaudio.org/) client. It
then requires a JACK server to run when it is launched and to be
connected to a MIDI synthetizer to produce music.

### Tokens

There are five kinds of tokens currently supported, that appear in the
following order in the arpeggigon's GUI:
* ricochet token: plays a note and reorients the play head toward the
  direction it's pointing to,
* start token: like the ricochet token but generated an impulsion when
  the machine starts,
* stop token: plays a note and makes the play head disappear,
* absorb token: like the stop token but doesn't play any note,
* split token: splits the impulsion into five.

### Multiple layers

It is possible to have multiple layers running in parallel, each one
having it's own control settings. Adjustable parameters include:
* the layer beat,
* the strength at which notes should be played,
* the volume,
* the instrument,
* the beats per bar for the given layer,
* the possibility to make the layer restart automatically after a
  certain number of bars, specifying if the layer should completly
  restart or not.

### Note attributes

Each tile, whether it's inhabited by a token or note is attributed a
repeat counter, telling how long a play head should stay on it.

For tiles inhabited by a token giving the possibility of a note being
played, it is possible to adjust some parameters, including:
* the note duration,
* an articulation, allowing the note to be played stronger depending
  on its position in the bar,

**Warning:** *Though it appears a slide can be specified, it is for
now a missing feature.*

### Button semantics

The start/restart button has a particular semantic. Pressing the start
button makes the machine start and turns the button into a restart
button. When pressed it will restart all the layers but take into
account the layer restart policy to know if it should completely
restart the layer or simply add the start heads to the already running
ones.

The stop button simply stops all the layers.

### Missing features

This lists the buttons that have no effect but are yet present at the
moment in the GUI. This features are being worked on in priority.
* Recording a performance is not yet possible.
* Configuration saving and loading is unavailable.
* Slides are not implemented.
* Tabs are not really named.

### Future features

This lists the features that would be nice to add to the arpeggigon at
some point:
* a nicer GUI, with the possibility to display note names on the
  board, and see which note is being focused on.
* a new “random split token”, that would redirect a play head in a
  random direction, or more generally split it in a random way.
* an ALSA backend for Linux.

[reacTogon]:https://www.youtube.com/watch?v=AklKy2NDpqs

Copyright and license
---

Copyright 2016, Guerric Chupin and Henrik Nilsson.

The arpeggigon is licensed under the BSD 3-Clause License. A copy of
the license is included in the [LICENSE](LICENSE) file.
