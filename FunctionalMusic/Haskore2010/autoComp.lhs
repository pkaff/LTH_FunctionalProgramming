\title{Functional Music}

\section{Introduction}
This report is a product of a home assignment in the course EDAN40, Functional Programming at Lunds Tekniska Högskola. The task was
to create a melody, using Haskore, and to that melody add an accompaniment. The melody that we were given to work on was Twinkle Twinkle
Little Star. The steps of the task was as follows: 1) Program the melody in Haskell using Haskore. 2) Generate
a bassline through a self-written function autoBass. 3) Generate a line of chords through a self-written function
autoChord. 4) Combine the autoBass and autoChord to make a function autoComp which adds both bass line and
chords to a melody. 5) Use this method on at least one other song.

For more information about Haskore and how to use it we recommend \href{http://fileadmin.cs.lth.se/cs/Education/EDAN40/assignment2/tutorial.pdf}{Haskore tutorial}. Even though this report intends to be self-explanatory we will
use several standard functions from the Haskore library and it is required to have Haskore to be able to test our code out.

\section{Generating the melody}
We will here begin with generating the melody for Twinkle. First we need some auxiliary functions, for example ones that give notes 
volumes so that we don't have to specify the volume for each single note.

\begin{code}
module AutoComp where

import Haskore hiding(chord, Key) --Need the names
import Data.List --We might need these

-- note updaters for mappings
fd d n = n d v
vol  n = n   v
v      = [Volume 80]
lmap f l = line (map f l)

-- repeat something n times
times  1    m = m
times n m = m :+: (times (n - 1) m)

\end{code}

The function fd takes a duration d aswell as a function, n, taking a duration and a list of note attributes and producing another data
type (usually Music). fd then gives the given Note a constant volume of 80.

The function lmap simply maps the function over a piece of Music and aligns it afterwards. The function line takes a [Music] and creates
a single piece of Music, where the components of the list are aligned in order.

We have a function times which simply repeats a piece of Music n times.

For those not so familiar with Haskore we quickly repeat the most essential basics. Music is a data type consisting (mostly, in this report) of a Note and [NoteAttributes]. A note itself consists of a 
pitch and a duration. Thus we can unambiguously determine each note that we would like to play in our music piece. An example of Music is (C, 4) 1/4 [] which gives us a C in octave 4 with duration 1/4 of a bar (no special attributes)
We can take a look at the complete data type Music along with the definitions for Pitch and Octave (commented out because it's already imported):

\begin{code}
--type Pitch      = (PitchClass, Octave)
--data PitchClass = Cf | C | Cs | Df | D | Ds | Ef | E | Es | Ff | F | Fs
--                | Gf | G | Gs | Af | A | As | Bf | B | Bs
--     deriving (Eq,Ord,Ix,Show,Read)
--type Octave     = Int

--data Music = Note Pitch Dur [NoteAttribute]   -- a note \ atomic 
--           | Rest Dur                         -- a rest /    objects
--           | Music :+: Music                  -- sequential composition
--           | Music :=: Music                  -- parallel composition
--           | Tempo  (Ratio Int) Music         -- scale the tempo
--           | Trans  Int Music                 -- transposition
--           | Instr  IName Music               -- instrument label
--           | Player PName Music               -- player label
--           | Phrase [PhraseAttribute] Music   -- phrase attributes
--    deriving (Show, Eq)

--type Dur   = Ratio Int                        -- in whole notes
\end{code}

As you can see Music can be lots of different things, but in this task it will mostly be Notes or Music that's put together. A Rest is when we don't play anything and this type will come in handy as well.

Furthermore we have a special set of operators, :+: and :=:, which operates on pieces of Music. :+: concatenates two pieces of music while :=: plays them simultaneously.

Now we can get started with our melody:

\begin{code}

--Twinkle melody

--first 2 bars
v1 = v1a :+: v1b 
--first 6 notes
v1a = lmap (times 2) (map (fd qn) [c 5, g 5, a 5]) 
--7th note (half note)
v1b = lmap (fd hn) [g 5] 

--next 2 bars
v2 = v2a :+: v2b 
v2a = lmap (times 2) (map (fd qn) [f 5, e 5, d 5])
v2b = lmap (fd hn) [c 5]

--first 2 bars of middle system
v3 = v3a :+: v3b 
v3a = lmap (times 2) (map (fd qn) [g 5, f 5, e 5])
v3b = lmap (fd hn) [d 5]

v12 = v1 :+: v2
mainVoice = v12 :+: times 2 v3 :+: v12

--Putting it all together:
twinkleSong = Instr "piano" (Tempo 3 (Phrase [Dyn SF] mainVoice))

\end{code}

Since the melody is composed of repetitions we extract the parts that repeat and make them separate subsections of the whole melody.
The subsections are then just put together in the correct order with the proper repetitions. Since Twinkle consists of many quarter notes
repeated twice each time they occur, the method of linemapping (times 2) onto the list of Music, generated by mapping (fd qn) on the notes
that we need, is preferred. This way we don't need as many repetitions in the code itself. The function (fd qn) simply takes a pitch and 
an octave and produces a note (in this case qn = quarter note) with volume 80 as said before.

The melody can be tested using functions from the Haskore library (e.g. test twinkleSong would produce a .midi file of the twinkle melody).

\section{Generating the bass line}

Let's get started with that bass line!

First thing's first; we need to define some types that we need to use in order to determine what bass notes are to be played.

\begin{code}
--Types
type Chord = (Pitch, Mode)
type ChordProgression = [(Chord, Dur)]
type Key = (PitchClass, Mode)
type BassLine =  Key -> (Chord, Dur) -> Music

\end{code}

So we define a ChordProgression to simply be a list where each chord has it's own duration. Each Chord is defined by it's root and it's Mode, i.e. Major or Minor, thus we can build a Major or a Minor chord
starting from the root given the Pitch for the root and the Mode. The Mode type is included in Haskore. The type Key is just a PitchClass combined with a mode (say (C, Major)) and denotes from which scale of tones
we are to look for notes when creating chords or basslines in this certain key. BassLine is a custom data type which simply denotes the type needed to create a bass line later on.

\begin{code}
--Transpose according to a key
keyTrans :: Key -> Int -> Pitch -> Pitch
keyTrans _ 0 pit = pit
keyTrans (pc, m) i (p, o)
	| m == Major = trans (transpositionsIntervalMajor 0 !! ((findIndex (p, 4) transpositionsMajor) + (i - 1))) (pc, o)
	| m == Minor = trans (transpositionsIntervalMinor 0 !! ((findIndex (p, 4) transpositionsMinor) + (i - 1))) (pc, o)
	where 
		transpositionsMajor = [f (pc, 4) | f <- map trans (transpositionsIntervalMajor 0)]
		transpositionsMinor = [f (pc, 4) | f <- map trans (transpositionsIntervalMinor 0)]
		findIndex p (x:xs)
			| x == p = 0
			| otherwise = 1 + findIndex p xs
	
transpositionsIntervalMajor n = map (+(12*n)) [0, 2, 4, 5, 7, 9, 11] ++ transpositionsIntervalMajor (n + 1)
transpositionsIntervalMinor n = map (+(12*n)) [0, 2, 3, 5, 7, 8, 10] ++ transpositionsIntervalMinor (n + 1)


\end{code}

Before we start note that transpositions in semitones are different depending on the mode of the chord or bassline and the key of the song. Thus we have created the function keyTrans which simply takes a 
key, an int and a pitch and transposes it correctly in the given key. To accomplish this we use the list transpositionsIntervalMajor/Minor to create a list of the number of semitones by which the given pitch
should be transposed. Currently this only works for major, Ionian, and minor, Aeolian, but other scales can be implemented.

Now consider the following code:

\begin{code}
data BassStyle = 
	Basic {bassLine :: BassLine } 
	| Calypso {bassLine :: BassLine }
	| Boogie {bassLine :: BassLine }
	
--Generates bassline depending on style and ChordProgression
autoBass :: BassStyle -> Key -> ChordProgression -> Music
autoBass b k cp = line (map ((bassLine b) k) cp)

basicBass :: Key -> (Chord, Dur) -> Music
basicBass _ (_, d) | d <= 0 = Rest 0
basicBass k (c, d) = cut d infList
	where 
		b = octaveDown (fst c)
		infList = Note b hn [] :+: Note (keyTrans k 5 b) hn [] :+: infList

calypsoBass :: Key -> (Chord, Dur) -> Music
calypsoBass _ (_, d) | d <= 0 = Rest 0
calypsoBass k (c, d) = cut d infList
	where 
		b = octaveDown (fst c)
		infList = times 2 (Rest qn :+: Note b en [] :+: Note (keyTrans k 3 b) en []) :+: infList
	
boogieBass :: Key -> (Chord, Dur) -> Music
boogieBass _ (_, d) | d <= 0 = Rest 0
boogieBass k (c, d) = cut d infList
	where 
		b = octaveDown (fst c)
		infList = times 2 (Note b en [] :+: Note (keyTrans k 5 b) en [] :+: Note (keyTrans k 6 b) en [] :+: Note (keyTrans k 5 b) en []) :+: infList

octaveDown :: Pitch -> Pitch
octaveDown (pc, 0) = (pc, 0)
octaveDown (pc, o) = (pc, o-1)

octaveUp :: Pitch -> Pitch
octaveUp (pc, o) = (pc, o+1)

\end{code}

autoBass is our main bass generator; given a BassStyle (in this assignment we can have either Basic, Calypso or Boogie), a Key and a ChordProgression it will create a bass line with the type Music.
The 3 different bass styles gives us 3 different approaches on how to build our bass line. The different auxiliary functions for each specific base type is simply implemented by creating an infinite list
of notes from the pattern defined by the given bass style and then cutting it at the supplied duration, i.e. the duration of the chord taken from the chord progression. Note that the auxiliary functions take
a tuple with Chord and Dur which is the list elements of a ChordProgression. For example the basic bass pattern is that we want to generate two half notes played in sequence with the first being the root
and the second being a fifth in the given key above the root. 

A side note for those that are not familiar with the basics of Haskore: cut (cut :: Dur -> Music -> Music) is a function which truncates pieces of Music down to the given duration.

Moving on; the calypsoBass is a bit more complicated. The pattern for the calypsoBass is a Rest for 1/4 of a bar then play 2 notes with duration 1/8, the root and the third of the chord. This should be repeated so that the
bass line covers an entire bar. Musically talented people will realize that the third depends on the mode Major or Minor (transposing 4 or 3 semitones up respectively), and this is solved in a smooth fashion
by using the keyTrans function, which transposes correctly in the given key. As you may or may not know Twinkle Twinkle contains only Major chords, but we will use another example which does contain minor
chords later on. Otherwise there's nothing special with this function except that we can note the use of times 2 where we want to repeat our bass line.

boogieBass. The bass of basses. I suppose. Eh, maybe not. But it's more fun and interesting to listen to than basic and calypso. It consists of 8 1/8 notes; root, fifth, sixth, fifth and repeat. 

If you were wondering about the octaveDown function all along it's simply a function reducing the octave from the given ChordProgression so that the bass line sounds more bassy. This might be a flaw in the way we defined
ChordProgression, but so be it! We take the opportunity to define the function octaveUp aswell, but we are not sure whether we will use it yet.

\section{Generating the accompaniment}

We almost have a complete song with melody and bassline but we still lack the chords. So here they come (commented out because we will improve the function autoChord later to take care of voicing):

\begin{code}
--Generates chord accompaniment with proper voicing (voicing needs to be implemented!)
-- autoChord :: Key -> ChordProgression -> Music
-- autoChord k [] = Rest 0
-- autoChord k (c:cp) = chordToMusic (fst c) (snd c) :+: autoChord k cp
	
-- chordToMusic :: Chord -> Dur -> Music
-- chordToMusic (p, m) d
	-- | m == Major = Note p d []:=: Trans 4 (Note p d []) :=: Trans 7 (Note p d [])
	-- | m == Minor = Note p d []:=: Trans 3 (Note p d []) :=: Trans 7 (Note p d [])

\end{code}

autoChord is the twin-function of autoBass. All we need to do is make every chord into Music and play them sequentially. Here, in the chordToMusic function, we can see the difference between a Major and a Minor chord. This function
works but is musically a bit "ugly". A chord can be played in many different inversions and some inversion sound better together than other. For example a C major chord could look like (in Haskell): Note (C, 4) hn [] :=:
Note (E, 4) hn [] :=: Note (G, 4) hn [] but it could also be written (played) as: Note (E, 4) hn [] :=: Note (G, 4) hn [] :=: Note (C, 5) hn []. Same notes, C E and G, are included in both chords but one has the C on the bottom
and the other has the C on the top. Knowing which sounds better is a matter of musical talent, but there are some guidelines. 1) Keep the chord notes within a certain limited range (apparently (E, 4) to (G, 5) is suitable for this
assignment). 2) Make the changes of the notes from one chord to another as small as possible. If you want to change from a C Major to a F Major chord and we play the C as (C, 4), (E, 4), (G, 4) we probably want something like
(C, 4), (F, 4), (A, 4) which is a F Major chord and we make as small changes as possible. This is not yet possible with the above code; that code will generate (F, 4), (A, 4), (C, 5) and it will sound a bit jumpy.

To test all of this out we must manually define the ChordProgression for Twinkle Twinkle.

\begin{code}
--Chordprogression for twinkle twinkle
twinkleChords :: ChordProgression
twinkleChords = cProg ++ fC ++ gC ++ gC ++ cG ++ cG ++ cG ++ cG ++ cProg ++ fC ++ gC ++ gC

cProg =[(((C, 4), Major), wn)]

fC = [(((F, 3), Major), hn), (((C, 4), Major), hn)]

gC = [(((G, 3), Major), hn), (((C, 4), Major), hn)]

cG = [(((C, 4), Major), hn), (((G, 3), Major), hn)]

\end{code}

We simply stick 2 chords which forms a bar together; the list fC contains a Fmajor chord and a Cmajor chord, both 1/2 long. Then we just concatenate the lists to make our song. There are definitely flaws in
the way we implement this - we must manually define all different chords in the case that we want a new song. One could instead define a general function which takes strings and converts them into chords, 
but that is for someone else to implement.

For the finishing touches we will generate good voicing for the chords. We thus need some function that will minimize the amount of semitones we change when going from one chord to another. We use the notion of pitch
representation where each chord can be represented as the number of semitones the three tones of the chord are in relation to a certain Pitch. For example [0, 4, 7] would be a pitch representation of any Major chord
in root position (with the root as the first tone). We can then invert this to make [0, 3, 8] (a Major chord where we move from root position to instead have the second tone of the chord as the lowest tone of the chord) or
[0, 5, 9] (a Major chord where we have the third tone as the lowest). Inverting [0, 5, 9] would then bring us back to [0, 4, 7]. We can create a function which inverts a chord in pitch representation as so (this is taken
directly from a non-standard library in the Haskore tutorial mentioned in the beginning of this report):

\begin{code}
type PitchRepresentation = (Pitch, [Int])

pitInvert (p1:p2:ps) = 0 : map (subtract p2) ps ++ [12 - p2]

\end{code}

Here we also introduced a new type, PitchRepresentation, which uniquely defines a chords position (not duration) given a certain Pitch and its pitch representation as a vector of Ints. The [Int] takes care of Major and Minor.

What we must do now is to convert the Chords to proper PitchReprepresentations which will yield a proper voicing. We will make everything start from the base chord of the key (hey look, the Key didn't turn out to be
totally ignored after all), so for Twinkle we have C Major or ((C, 4), [0, 4, 7]), so whenver we run into this particular chord we have a basic triad with distinct position.

\begin{code}
--Creates an accompaniment to a given ChordProgression
autoChord :: Key -> ChordProgression -> Music
autoChord k [] = Rest 0
autoChord k (c:cp) = chordToMusic k (fst c) (snd c) :+: autoChord k cp

--Base chord is the pitch class from the key in root position. Otherwise we find out which chord is the closest to this base chord
chordToMusic :: Key -> Chord -> Dur -> Music
chordToMusic (pc, m) ch d
	| pc == fst (fst ch) = baseChord
	| otherwise = pitRepToMusic (nearestPitRep (keyToChord (pc, m)) ch) d
	where baseChord = Note (pc, 4) d []:=: Trans 4 (Note (pc, 4) d []) :=: Trans 7 (Note (pc, 4) d [])

--Returns closest PitchRepresentation of the first chord in relation to the first chord
nearestPitRep :: Chord -> Chord -> PitchRepresentation
nearestPitRep baseChord ch = smallestAbsPitCompare (chordToPitRep baseChord) (allPitRep (chordToPitRep ch) (length (snd (chordToPitRep ch))))

--Compares the list of PitchRepresentations to the first PitchRepresentation and returns the smallest one
smallestAbsPitCompare :: PitchRepresentation -> [PitchRepresentation] -> PitchRepresentation
smallestAbsPitCompare basePitch xs = xs !! (head (elemIndices (minimum list) list))
	where list = map abs ((map ((-) (pitRepToAbsPitch basePitch)) (map pitRepToAbsPitch xs)))

--Converts PitchRepresentation to AbsPitch ([0, inf), 0 being (C, 0))
pitRepToAbsPitch :: PitchRepresentation -> AbsPitch
pitRepToAbsPitch (p, xs) = sum [absPitch (trans x p) | x<-xs]

--Gives all PitchRepresentations in a list
allPitRep :: PitchRepresentation -> Int -> [PitchRepresentation]
allPitRep _ 0 = []
allPitRep (p, xs) l
	| l == (length xs) = (p, xs):allPitRep (p, xs) (l - 1)
	| otherwise = (transform p, pitInvert xs):allPitRep (transform p, pitInvert xs) (l - 1)
	where transform = trans (xs !! 1)

--Converts Chord to PitchRepresentation
chordToPitRep :: Chord -> PitchRepresentation
chordToPitRep (p, m)
	| m == Major = (p, [0, 4, 7])
	| m == Minor = (p, [0, 3, 7])
	
--Converts PitchRepresentation with certain duration to Music
pitRepToMusic :: PitchRepresentation -> Dur -> Music
pitRepToMusic (p, []) d = Rest 0
pitRepToMusic (p, (x:xs)) d = Trans x (Note p d []) :=: pitRepToMusic (p, xs) d
	
--Just give the key an octave
keyToChord :: Key -> Chord
keyToChord (pc, m) = ((pc, 4), m)

\end{code}

That was quite a few help functions just for the case of getting a nice voicing. Oh well. chordToMusic now creates a base chord depending on the key, and we will compare the rest of the chords to this chord when determining
which inversion of the chord is the closest. Our type PitchRepresentation comes in handy when we want to represent the chord in a more explicit way. You can follow the code and read the comments to get an idea of what's going on.

Now we just create a bass line and an accompaniement for the twinkle melody that we produced a long long time ago:

\begin{code}
--Bassline for twinkle twinkle
bass = autoBass (Boogie boogieBass) (C, Major) twinkleChords
twinkleBass = Instr "bass" (Tempo 3 (Phrase [Dyn SF] bass))

--Comp for twinkle twinkle
comp = autoChord (C, Major) twinkleChords
twinkleComp = Instr "piano" (Tempo 3 (Phrase [Dyn SF] comp))

autoComp :: BassStyle -> Key -> ChordProgression -> Music
autoComp bs k cp = (autoBass bs k cp) :=: (autoChord k cp)

--Bass+Melody+Chords for twinkle twinkle
twinkleFull = mainVoice:=:bass:=:comp
playTwinkle = Instr "piano" (Tempo 2 (Phrase [Dyn SF] twinkleFull))

\end{code}

To test this out you need to have this file in the same file as the Haskore library and write :l autoComp in your ghci to load this module. Then to listen to the twinkle song you use test playTwinkle to create a
.midi file which you can listen to. Don't forget that you can change the bass style and play around a bit.

\section{Testing another song out}

You thought we were done here? So did we, but the last task is to take another song of your own choice and let the program create an accompaniment for it. We'll choose "Lilla snigel akta dig". If you doubt Daniels notereading-skills
you might want to look the chords and notes up. We'll simplify all the stuff that's not only Major or Minor. Let's start with the chords:

\begin{code}
--Chordprogression for Lilla snigel
snigelChords :: ChordProgression
snigelChords = cProg ++ cProg ++ dmG ++ cProg ++ cProg ++ cAm ++ dmG ++ cProg

dmG = [(((D, 4), Minor), hn), (((G, 3), Major), hn)]

cAm = [(((C, 4), Major), hn), (((A, 3), Minor), hn)]

--Comp for snigel
snigelComp = autoChord (C, Major) snigelChords
snigelAccomp = Instr "piano" (Tempo 3 (Phrase [Dyn SF] snigelComp))

\end{code}

Melody:

\begin{code}

--Snigel melody

--first 2 bars
s1 = s1a :+: s1b 
--first 6 notes
s1a = lmap (times 2) (map (fd qn) [g 5, e 5, g 5]) 
--7th note (half note)
s1b = lmap (fd hn) [e 5] 

--third bar
s2 = s2a :+: s2b 
s2a = lmap (times 2) (map (fd qn) [f 5])
s2b = lmap (fd hn) [d 5]

--fourth bar is transposition of third
s3 = Trans 2 s2

--7th bar
s4 = lmap (times 2) (map (fd qn) [f 5, d 5])

--8th bar
s5 = lmap (fd wn) [c 5]

snigelMainVoice = s1 :+: s2 :+: s3 :+: s1 :+: s4 :+: s5

--Putting it all together:
snigelSong = Instr "piano" (Tempo 3 (Phrase [Dyn SF] snigelMainVoice))

\end{code}

Bass line and finished product:

\begin{code}

--Bassline for lilla snigel
snigelBassLine = autoBass (Calypso calypsoBass) (C, Major) snigelChords
snigelBass = Instr "bass" (Tempo 3 (Phrase [Dyn SF] snigelBassLine))

--Bass+Melody+Chords for lilla snigel
snigelFull = snigelMainVoice:=:snigelBassLine:=:snigelComp
playSnigel = Instr "piano" (Tempo 2 (Phrase [Dyn SF] snigelFull))

\end{code}

Trying this song out is analogous to the Twinkle Twinkle case: type test playSnigel in your ghci after loading and it should generate a .midi file which you can listen to through timidity or some other .midi player. 
Wonderful, innit? That would be all for this report, thank you for reading :)

Fin

--Number of hours:
-- 3,5x2 hours
-- 0,5*2 hours
-- 1 hour
-- 1,5*2 hours
-- 2*2 hours
-- 1hour
-- 1,5 hours
-- 9 hours