Reactogon Design
================

2016-05-18: INCOMPLETE, but should give an idea. Refine/structure better
as see fit.

Target levels
-------------
- [ ] Baseline: text interface only, not necessarily (very) interactive,
      but at the very least able to record a performance as a MIDI file.
- [ ] Basic system with a GUI running on a Linux desktop. At least able
      to generate sound
- [ ] Full-featured system with GUI running on a Linux desktop.
- [ ] Mobile version, subject to cooperation with Ivan/Keera Studios

Feature Summary
---------------

* Modelled after the original Reactogon
* Up to 16 layers. Each layer an extended version of the original Reactogon.
  Extensions include:
  - Each tile has an associated "repeat count" n. The play head stays
    on the title for n clock ticks, repeating any basic action for
    each tick, before moving on (possibly in new direction).
    Specifically, this means that n = 1 is the normal behaviour and
    n = 0 means the play head moves on immediately without performing any
    action.
  - User-defined "groove" per layer. E.g. every other beat can be delayed for
    a swing feel.
  - Accented (more strongly played) notes.
  - Slide (kind of slide *could* even depend on direction of the play head!)
    (not going to work well for polymorphic layers probably. But the MIDI
    translator could simply ignore slide messages until the current one is
    done. Then everything OK as long as no overlapping slides.)
  - MIDI CC/program change can be sent for a note
  - Two tempo-synched LFOs per layer
  - Bank and Program
  - Volume slider
  - Pan pot
  - Layer transposition
  - A layer can be set to be unpitched: the pitch of all output notes
    are set to a specific pitch (useful for e.g. rythm layers).
  - Muting and Soloing of layers
* MIDI integration
  - synchronization to external MIDI clock if selected and available
  - start, stop, continue: send and receive
  - each layer transposed in response to note on message on designated channel
  - designated MIDI channel for each layer for output:
    + note on/off
    + program change
    + volume
    + pan
    + mapping of LFOs to specific MIDI CC (including volume or pan which
      overrides the static values).
    + pitch bend
* Dynamic addition/deletion of layers(?) Or simply a fixed number of layers?
* Overall control/performance tab. For ecach layer:
  - Volume
  - Pan
  - Mute (inhibits all MIDI out messages, but layer continues to run)
  - Solo (messages from all other layers inhibited, but mute status
    preserved; only one layer at a time can be soloed)
  These controls duplicated on each layer
* Saving & loading of configurations
* Loading of individual layers from stored configurations.
* Directly recolrding to/generating a MIDI file. Record button?

Clocking
--------
The system beat, relative to which the overall tempo is given (in BPM)
is defined to be one quarter note.

The layer beats is specified separately but defaults to one quarter note.

A beats per par is also specified per layer. Together with the layer beat
this gives the time signature (of sorts) for a layer.

The system tempo is either set internally, in BPM, or synched to an external
MIDI clock.

Sufficiently flexible swing could be specified per layer by specifying
how much earlier or later beats 2, 3 and 4 should be relative to a straight
beat, i.e. three numbers between say -90% and 90%. A standard swing
beat would thus be given by 50, 0 50.

Only beat numbers as defined by the layer's beats per bar would be used,
and beat numbers beyond 4 would be played straight.

Or one might want to allow the number of numbers to be beats per bar minus 1.

Overall Structure
-----------------
                              ------------------
                              |                |
                Model <----> GUI <-----        |
                   |                  |        |
                   |--> Global-+----->|        |
                   |           V      |        V
                   +------> Layers -----> MIDI translator ---> MIDI Synth
                   |
    MIDI keyboard --
                   |
    (MIDI clock) ---

Gloal, Layers, and MIDI translator are all FRP (Yampa) moduled.

  * Global (better name needed!) takes care of system-wide signal
    processing, e.g. definition of the system clock, run/stop logic?

  * Each Layer is a Reactogon. The core semantics is defined in
    BasicSemantics. The intention is to lift (most of) this into
    an event-processing signal fnction using "accumBy". Additional
    signal functions per layer would be needed to define the
    layer clock, layer LFOs, and maybe more.

  * The MIDI translator is responsible for translating the collective
    output from the layers (high-level MIDI notes, LFO output) into
    low level MIDI events. This involves things like ensuring that a
    note off event is scheduled for every note on event at the appropriate
    time, translating and realising continuous control signals
    (from LFOs, slide ornaments) into discrete MIDI messages,
    and merging it all into a single sequence of events, taking the
    limited capacity of a standard MIDI connection into account
    (more corse grained translation of continous control signals
    if too busy? dropping notes in case of excessive polyphony per
    channel and/or overall?)

  * The GUI manipulates a model of the system tailored to the needs of the
    GUI. Control signals are derived from this for Global and each
    layer (e.g. the configuration of a board, the board transposition).

    The GUI also needs to observe the output from Global and Layers
    to animate layers as the play heads move around and to display
    the system tempo in case this is derived from external MIDI clock.

    The GUI also needs to talk to the MIDI translator to send e.g.
    configured prgram change, bank, volume and pan messages for each
    layer whenever the Reactogon is started.

    Muting of layers could also be achived this way by instructing
    the translator to temporarily stop listening to a muted layer.
    (Muted layers would still run and be animated.)
