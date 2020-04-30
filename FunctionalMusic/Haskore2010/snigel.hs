module Snigel where

import Haskore hiding(chord, Key) --Need the names
import Data.List --We might need these
import AutoComp

-- --Snigel melody

-- --first 2 bars
-- s1 = s1a :+: s1b 
-- --first 6 notes
-- s1a = lmap (times 2) (map (fd qn) [g 5, e 5, g 5]) 
-- --7th note (half note)
-- s1b = lmap (fd hn) [e 5] 

-- --third bar
-- s2 = s2a :+: s2b 
-- s2a = lmap (times 2) (map (fd qn) [f 5])
-- s2b = lmap (fd hn) [d 5]

-- --fourth bar is transposition of third
-- s3 = Trans 2 s2

-- --7th bar
-- s4 = lmap (times 2) (map (fd qn) [f 5, d 5])

-- --8th bar
-- s5 = lmap (fd wn) [c 5]

-- snigelMainVoice = s1 :+: s2 :+: s3 :+: s1 :+: s4 :+: s5

-- --Putting it all together:
-- snigelSong = Instr "piano" (Tempo 3 (Phrase [Dyn SF] snigelMainVoice))

-- --Chordprogression for Lilla snigel
-- snigelChords :: ChordProgression
-- snigelChords = cProg ++ cProg ++ dmG ++ cProg ++ cProg ++ cAm ++ dmG ++ cProg

-- dmG = [(((D, 4), Minor), hn), (((G, 3), Major), hn)]

-- cAm = [(((C, 4), Major), hn), (((A, 3), Minor), hn)]



-- --AutoComp
-- autoComp :: Key -> ChordProgression -> Music
-- autoComp k cp = autoBass k cp :=: autoChord k cp

-- snigelCompTest = Instr "piano" (Tempo 3 (Phrase [Dyn SF] (autoComp (C, Major) snigelChords)))