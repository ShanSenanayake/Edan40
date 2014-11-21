module AutoComp where
import Haskore

data BassStyle = Basic | Calypso | Boogie deriving (Eq)
data Chord = Major Pitchclass | Minor Pitchclass deriving (Eq)


type ChordProgression = [(Chord,Dur)]

bassLine :: BassStyle -> [NoteAttribute]->[Pitch]-> [Music]
bassLine Basic vol = basicBassLine vol 0
bassLine Calypso vol = calypsoBassLine vol 0
bassLine Boogie vol = boogieBassLine vol (-1)


basicBassLine ::  [NoteAttribute]-> Int-> [Pitch] -> [Music]
basicBassLine vol 0 m = (Note  (m!!0) hn vol):(basicBassLine vol 4 m)
basicBassLine vol 4 m = (Note  (m!!4) hn vol):(basicBassLine vol 0 m)
basicBassLine vol _ m = []


boogieBassLine ::  [NoteAttribute]-> Int-> [Pitch] -> [Music]
boogieBassLine vol (-1) m = (hnr):(boogieBassLine vol 0 m)
boogieBassLine vol 0 m = (Note (m!!0) qn vol):(boogieBassLine vol 2 m)
boogieBassLine vol 2 m = (Note (m!!2) qn vol):(boogieBassLine vol (-1) m)
boogieBassLine vol _ m = []


calypsoBassLine ::  [NoteAttribute]-> Int-> [Pitch] -> [Music]
calypsoBassLine vol 0 m = (Note (m!!0) qn vol):(Note (m!!4) qn vol):(calypsoBassLine vol 5 m)
calypsoBassLine vol 5 m = (Note (m!!5) qn vol):(Note (m!!4) qn vol):(calypsoBassLine vol 0 m)
calypsoBassLine vol _ m = []


