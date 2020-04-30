module Twinkle where

import Haskore hiding(chord, Key) --Need the names
import Data.List --We might need these
import AutoComp

-- --Twinkle melody
-- v1 = v1a :+: v1b --first 2 bars
-- v1a = lmap (times 2) (map (fd qn) [c 5, g 5, a 5]) --first 6 notes
-- v1b = lmap (fd hn) [g 5] --7th note (half note)

-- v2 = v2a :+: v2b --next 2 bars
-- v2a = lmap (times 2) (map (fd qn) [f 5, e 5, d 5])
-- v2b = lmap (fd hn) [c 5]

-- v3 = v3a :+: v3b --first 2 bars of middle system
-- v3a = lmap (times 2) (map (fd qn) [g 5, f 5, e 5])
-- v3b = lmap (fd hn) [d 5]

-- v12 = v1 :+: v2
-- mainVoice = v12 :+: times 2 v3 :+: v12

-- --Putting it all together:
-- twinkleSong = Instr "piano" (Tempo 3 (Phrase [Dyn SF] mainVoice))

-- --Chordprogression for twinkle twinkle
-- twinkleChords :: ChordProgression
-- twinkleChords = cProg ++ fC ++ gC ++ gC ++ cG ++ cG ++ cG ++ cG ++ cProg ++ fC ++ gC ++ gC

-- cProg =[(((C, 4), Major), wn)]

-- fC = [(((F, 3), Major), hn), (((C, 4), Major), hn)]

-- gC = [(((G, 3), Major), hn), (((C, 4), Major), hn)]

-- cG = [(((C, 4), Major), hn), (((G, 3), Major), hn)]

-- --AutoComp
-- autoComp :: Key -> ChordProgression -> Music
-- autoComp bs k cp = autoBass Basic k cp :=: autoChord k cp

-- twinkleCompTest = Instr "piano" (Tempo 3 (Phrase [Dyn SF] (autoComp (C, Major) twinkleChords)))