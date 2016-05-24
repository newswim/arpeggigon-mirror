-- Basic Semantics for a Reactive Music Cellular Automaton.
-- Inspired by the reacTogon.
-- Written by Henrik Nilsson, 2016-05-03
-- Based on an earlier version.
--
-- This gives the semantics of a single Reactogon layer. The output is
-- a high-level representation of notes for each beat. This is to be
-- translated to low-level MIDI message by a subsequent translator
-- responsible for merging notes from different layers, ensuring that
-- a note off message corresponding to each note on message is always
-- emitted after the appropriate time, rendering any embellismnets
-- such as slides (while not generating too much MIDI data), etc.

module Reactogon.Semantics where

import Data.Array
import Data.Maybe (catMaybes)
import Data.List (nub, intersperse)
import Data.Ratio


------------------------------------------------------------------------------
-- Basic Type Synonyms
------------------------------------------------------------------------------

-- Unipolar control value; [0, 1]
type UCtrl = Double

-- Bipolar control value; [-1, 1]
type BCtrl = Double


------------------------------------------------------------------------------
-- Time and Beats
------------------------------------------------------------------------------

-- The assumption is that the automaton is clocked by a beat clock and
-- thus advances one step per beat. For an automaton working in real time,
-- the beat clock would be defined externally, synchronized with other
-- layers and possibly external MIDI, and account for tempo, any swing, etc.

-- Tempo

-- The tempo is the number of beats per minute.
type Tempo = Int

-- Beats and Bars

-- A beat in itself is not important.
type Beat = ()

-- Beats per Bar: number of beats per bar in the time signature of a layer.
-- Non-negative.
type BeatsPerBar = Int

-- The beat number in the time signature of the layer. The first beat is 1.
type BeatNo = Int

nextBeatNo :: BeatsPerBar -> BeatNo -> BeatNo
nextBeatNo bpb bn = bn `mod` bpb + 1


{-
-- Not needed for individual layers (at present)

-- Time; [0,+inf)
type Time = Double
-}


------------------------------------------------------------------------------
-- MIDI
------------------------------------------------------------------------------

-- This semantics mainly works with a high-level represemntation of notes.
-- But it is convenient to express some of the high-level aspects directly
-- in the corresponding MIDI terms to facilitate the translation.

-- MIDI note number; [0,127]
type MIDINN = Int


-- Assume MIDI convetion: 60 = "Middle C" = C4
middleC    = 60
middleCOct = 4


-- MIDI velocity; [0,127]
type MIDIVel = Int


-- MIDI Program Change: Program Number; [0,127]
type MIDIPN = Int


-- MIDI Control Change: Control Number and Control Value; [0,127]
type MIDICN = Int
type MIDICV = Int

-- MIDICVRnd gives the option to pick a control value at random.
-- (Handled through subsequent translation to low-level MIDI events.)
data MIDICVRnd = MIDICV MIDICV | MIDICVRnd deriving (Eq, Show)


------------------------------------------------------------------------------
-- Notes
------------------------------------------------------------------------------

-- Pitch

-- We chose to represent pitch by MIDI note number
newtype Pitch = Pitch MIDINN deriving Eq

pitchToMNN :: Pitch -> MIDINN
pitchToMNN (Pitch nn) = nn

instance Show Pitch where
    show (Pitch nn) = names !! note ++ show oct
        where
            nn'   = nn - middleC
            note  = nn' `mod` 12
            oct   = nn' `div` 12 + middleCOct
            names = ["C",  "C#", "D",  "D#", "E",  "F",
                     "F#", "G",  "G#", "A",  "A#", "B"]

-- Relative pitch in semi tones. Used for e.g. transposition.
type RelPitch = Int


-- Articulation

-- Each layer has a setting that indicate how strongly the notes
-- should normally be played as a percentage of full strength.
-- (In the real application, this settig can be set to a fixed value
-- or set to be derived from teh last input note, "as played").
-- Individual notes can tehn be accented (played more strongly),
-- either unconditionally or as a function of the beat count.

type Strength = UCtrl

-- This could of course be generalised, e.g. a list of beat numbers to
-- accentuate. But this is simple and accounts for the most common patterns.
data Articulation = NoAccent
                  | Accent
                  | Accent1
                  | Accent13
                  | Accent14
                  | Accent24
                  deriving (Eq, Show)

accentStrength = 1.2

-- Articulated strength
articStrength :: Strength -> BeatNo -> Articulation -> Strength
articStrength st bn art
    | accentedBeat = st * accentStrength
    | otherwise    = st
    where
        accentedBeat =
           case (bn, art) of
               (_, NoAccent) -> False
               (_, Accent)   -> True
               (1, Accent1)  -> True
               (1, Accent13) -> True
               (3, Accent13) -> True
               (1, Accent14) -> True
               (4, Accent14) -> True
               (1, Accent24) -> True
               (4, Accent24) -> True
               _             -> False


-- Duration

-- Duration in terms of a whole note at the *system* tempo. (Each
-- layer is clocked at a layer beat that is a fraction/multiple of the
-- system tempo). Note that notes are played a little shorter than
-- their nominal duration. This is taken care of by the translation
-- into low-level MIDI events. (One might consider adding indications
-- of staccato or tenuto.)
type Duration = Rational


-- Ornamentation

-- Notes can be ornamented. Traditionnally, ornamenting refers to modifications
-- of the pitch, such as a trill or a grace note. Here we use the term in
-- a generalised sense.
--   * A MIDI program change (to be sent before the note).
--   * A MIDI Continuous Controler Change (to be sent before the note).
--   * A Slide
-- One might also consider adding trills, grace notes, MIDI after touch ...

data Ornaments = Ornaments {
    ornPC    :: Maybe MIDIPN,
    ornCC    :: [(MIDICN, MIDICVRnd)],
    ornSlide :: SlideType
} deriving Show

data SlideType = NoSlide | SlideUp | SlideDn deriving (Eq, Show)


-- Notes

-- Attributes needed to generate a note.
--   * The pitch of a note is given by the position on the board
--   * The strength is given by the layer strength, beat no., and articulation
--   * Duratio and Ornamentatio are stored
data NoteAttr = NoteAttr {
    naArt :: Articulation,
    naDur :: Duration,
    naOrn :: Ornaments
} deriving Show


-- High level note representation emitted from a layer
data Note = Note {
    notePch :: Pitch,
    noteStr :: Strength,
    noteDur :: Duration,
    noteOrn :: Ornaments
} deriving Show


------------------------------------------------------------------------------
-- Board
------------------------------------------------------------------------------

-- Numbering; row number inside tile, column number below:
--     _   _
--   _/2\_/2\_
--  / \_/1\_/1\
--  \_/1\_/1\_/
--  / \_/0\_/0\
--  \_/0\_/0\_/
--    \_/ \_/
--    -1 0 1 2


-- Angle measured in multiples of 60 degrees.
type Angle = Int

data Dir = N | NE | SE | S | SW | NW deriving (Enum, Eq, Show)


turn :: Dir -> Angle -> Dir
turn d a = toEnum ((fromEnum d + a) `mod` 6)


type Pos = (Int, Int)

-- Position of neighbour in given direction
neighbor :: Dir -> Pos -> Pos
neighbor N  (x,y) = (x,     y + 1)
neighbor NE (x,y) = (x + 1, y + 1 - x `mod` 2)
neighbor SE (x,y) = (x + 1, y - x `mod` 2)
neighbor S  (x,y) = (x,     y - 1)
neighbor SW (x,y) = (x - 1, y - x `mod` 2)
neighbor NW (x,y) = (x - 1, y + 1 - x `mod` 2)


-- Position and transposition to pitch:
--   * Harmonic Table" layout: N = +7; NE = +4; SE = -3
--   * (0,0) assumed to be "Middle C"
posToPitch :: Pos -> RelPitch -> Pitch
posToPitch (x,y) tr =
    Pitch (y * 7 + x `div` 2 - 3 * (x `mod` 2) + middleC + tr)


-- Actions
-- Maybe this could be refined: some of the actions might be useful
-- both in note playing and silent versions: e.g. changing direction without
-- playing a note; playing a note without changing direction.

data Action = Inert              -- No action, play heads just move through.
            | Absorb             -- Remove play head silently.
            | Stop  NoteAttr     -- Play note then remove play head.
            | ChDir NoteAttr Dir -- Play note then change direction.
            | Split NoteAttr     -- Play note then split head into five new.
            deriving Show


-- Cells
-- A cell stores an action and a repetition number.
-- 0:     the cell is completely bypassed, as if it wasn't there.
-- 1:     the action is carried out once (default)
-- n > 1: any note output of the action is repeated (n-1) times before the
--        action is carried out.

type Cell = (Action, Int)


-- Make a cell with a default repeat count of 1.
mkCell :: Action -> Cell
mkCell a = mkCellRpt a 1


-- Make a cell with a non-default repeition number.
mkCellRpt :: Action -> Int -> Cell
mkCellRpt a n | n >= 0    = (a, n)
              | otherwise = error "The repetition number of a cell must be \
                                  \non-negative."


-- Board extent: south-west corner and north-east corner.
-- This covers most of the MIDI range: A#-1 (10) to G7 (103).
swc, nec :: Pos
swc = (-9, -6)
nec = (9, 6)


-- Test if a position is on the board as defined by swc and nec.
-- The assumption is that odd columns contain one more cell, as per the
-- picture above. Of course, one could opt for a "zig-zag" layout
-- with each column having the same number of cells which would be slightly
-- simpler.
onBoard :: Pos -> Bool
onBoard (x,y) =    xMin <= x && x <= xMax
                && yMin <= y
                && (if even x then
                        y < yMax
                    else
                        y <= yMax)
    where
        (xMin, yMin) = swc
        (xMax, yMax) = case nec of
                           (x, y) | even x    -> (x, y + 1)
                                  | otherwise -> (x, y)


type Board = Array Pos Cell


-- Build a board from a list specifying the non-empty cells.
makeBoard :: [(Pos, Cell)] -> Board
makeBoard pcs =
    array (swc,nec')
          ([(p, if onBoard p then mkCell Inert else mkCell Absorb)
           | p <- range (swc, nec')]
           ++ [(p,c) | (p, c) <- pcs, onBoard p])
    where
        -- This is to ensure (neighbor NW nec) is included on the board,
        -- regardless of whether the column of nec is even or odd.
        -- Otherwise, due to the "jagged" upper edge, the top row would
        -- be missing, but every other cell of that *is* on the board.
        -- The "superfluous" cells are set to Absorb above.
        nec' = neighbor N nec


-- Look up a cell
lookupCell :: Board -> Pos -> Cell
lookupCell b p = if onBoard p then (b ! p) else (Absorb, 1)


------------------------------------------------------------------------------
-- Play Head
------------------------------------------------------------------------------

-- A play head is characterised by:
--   * Current position
--   * Number of beats before moving
--   * Direction of travel
-- If an action involves playing a note, this is repeated once for
-- each beat the play head is staying, with the rest of the action
-- carried out at the last beat.

data PlayHead =
    PlayHead {
        phPos :: Pos,
        phBTM :: Int,
        phDir :: Dir
    }
    deriving (Eq, Show)


------------------------------------------------------------------------------
-- State transition
------------------------------------------------------------------------------

-- Advance the state of a single play head.
--
-- The result is a list of heads to be actioned at the *next* beat
-- later) and possibly a note to be played at *this* beat.

advanceHead :: Board -> BeatNo -> RelPitch -> Strength -> PlayHead
               -> ([PlayHead], Maybe Note)
advanceHead bd bn tr st ph = ahAux (moveHead bd ph)
    where
        ahAux ph@PlayHead {phPos = p, phBTM = btm, phDir = d} =
            case fst (lookupCell bd p) of
                Inert       -> ([ph], Nothing)
                Absorb      -> ([], Nothing)    -- No point waiting until BTM=0
                Stop na     -> (newPHs [], Just (mkNote p bn tr st na))
                ChDir na d' -> (newPHs [ph {phDir = d'}],
                                Just (mkNote p bn tr st na))
                Split na    -> (newPHs [ PlayHead {
                                             phPos   = p,
                                             phBTM   = 0,
                                             phDir   = d'
                                         }
                                       | a <- [-2 .. 2],
                                         let d' = turn d a
                                       ],
                                Just (mkNote p bn tr st na))
            where
                newPHs phs = if btm > 0 then [ph] else phs


-- Moves a play head if the BTM counter has reached 0, otherwise decrement BTM.
-- Any encountered cells where the repeat count is < 1 are skipped.
moveHead :: Board -> PlayHead -> PlayHead
moveHead bd (ph@PlayHead {phPos = p, phBTM = btm, phDir = d})
    | btm < 1   = let
                      p'   = neighbor d p
                      btm' = snd (lookupCell bd p')
                  in
                      moveHead bd (ph {phPos = p', phBTM = btm'})
    | otherwise = ph {phBTM = btm - 1}


mkNote :: Pos -> BeatNo -> RelPitch -> Strength -> NoteAttr -> Note
mkNote p bn tr st na =
    Note {
        notePch = posToPitch p tr,
        noteStr = articStrength st bn (naArt na),
        noteDur = naDur na,
        noteOrn = naOrn na
    }


-- Advance a list of heads, collecting all resulting heads and notes.
-- Any duplicate play heads are eliminated (or their number may uselessly
-- grow very quickly), and a cap (50, arbitrary, but should be plenty,
-- expecially given the board size) on the number of simultaneous playheads
-- per layer is imposed.
advanceHeads :: Board -> BeatNo -> RelPitch -> Strength -> [PlayHead]
             -> ([PlayHead], [Note])
advanceHeads bd bn tr st phs =
    let
       (phss, mns) = unzip (map (advanceHead bd bn tr st) phs)
    in
       (take 50 (nub (concat phss)), catMaybes mns)


-- Given an initial list of play heads, run a board until there are no
-- more heads (or "forever", if that does not happen). The result is
-- a list of all notes played for each pulse.
--
-- Note: The original reactogon has special start counters. An "internal"
-- board as defined here along with a list of inital read heads could
-- be derived from an "external" board representation more closely aligned
-- with the GUI represenatation.
--
-- In the real implementation:
--   * A layer beat clock would be derived from the system beat (as a
--     fraction/multiple, adding any swing) and each clock event be tagged
--     with the beat number.
--   * The board would not necessarily be a constant input. (One might
--     consider allowing editing a layer while the machine is running)
--   * The time signature and thus the beats per par would not necessarily
--     be a constant input (one might consider allowing changing it while
--     the machine is running, but perhaps not very useful).
--   * The transposition would be dynamic, the sum of a per layer
--     transposition that can be set through the user interface and the
--     difference between the MIDI note number of the last external
--     note received for the layer and middle C (say).
--   * The strength would be dynamic, configurable as either the strength
--     set through the user interface or the strength of the last external
--     note received for the layer (derived from its MIDI velocity).

runRMCA :: Board -> BeatsPerBar -> RelPitch -> Strength -> [PlayHead]
        -> [[Note]]
runRMCA _  _  _  _  []  = []
runRMCA bd bpb tr st phs = runAux 1 phs
    where
        runAux bn phs = ns : runAux (nextBeatNo bpb bn) phs'
            where
                (phs', ns) = advanceHeads bd bn tr st phs


-- Print played notes in a time-stamped (bar, beat), easy-to-read format.

ppNotes :: BeatsPerBar -> [[Note]] -> IO ()
ppNotes bpb nss = ppnAux (zip [(br,bn) | br <- [1..], bn <- [1..bpb]] nss)
    where
        ppnAux [] = return ()
        ppnAux ((_, []) : tnss) = ppnAux tnss
        ppnAux ((t, ns) : tnss) = do
            putStrLn ((leftJustify 10 (show t)) ++ ": "
                      ++ concat (intersperse ", " (map show ns)))
            ppnAux tnss


leftJustify :: Int -> String -> String
leftJustify w s = take (w - length s) (repeat ' ') ++ s

{-
------------------------------------------------------------------------------
-- Simple test
------------------------------------------------------------------------------

-- testBoard = makeBoard [((0,0),  mkCell (ChDir na1 N)),
--                        ((0,1),  mkCell (ChDir na1 SE)),
--                        ((1,1),  mkCell (Split na1)),
--                        ((1,-1), mkCell (Split na1)),
--                        ((-1,0), mkCell (ChDir na2 NE))]

testBoard = makeBoard [((0,0), mkCell (ChDir na1 N)),
                       ((0,2), mkCellRpt (ChDir na2 SE) 3),
                       ((2,1), mkCell (ChDir na1 SW)),
                       ((1,1), mkCellRpt (ChDir na1 N) 0) {- Skipped! -}]

na1 = NoteAttr {
          naArt = Accent13,
          naDur = 1 % 4,
          naOrn = Ornaments Nothing [] NoSlide
      }

na2 = NoteAttr {
          naArt = NoAccent,
          naDur = 1 % 16,
          naOrn = Ornaments Nothing [(10, MIDICVRnd)] SlideUp
      }

bpb :: Int
bpb = 4

main = ppNotes bpb (take 50 (runRMCA testBoard
                                     bpb
                                     0
                                     0.8
                                     [PlayHead (0,0) 1 N]))
-}
