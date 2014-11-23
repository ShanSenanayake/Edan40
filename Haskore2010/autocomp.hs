module AutoComp where
import Haskore 

data BassStyle = Basic | Calypso | Boogie deriving (Eq)
data Harmony = Dur | Moll deriving (Eq)
type Chord = (PitchClass,Harmony)
type KeyS = Chord
type ChordProgression = [(Chord,Dur)]

majorScale = [0,2,4,5,7,9,11]
minorScale = [0,1,3,5,7,8,10]

generatePitchScale :: KeyS -> Octave -> Chord -> [Pitch]
generatePitchScale (tone, Dur) octave (start, _) = map pitch (map ((absPitch (tone,octave))+) (shift (tone,octave) (start,octave) majorScale))
generatePitchScale (tone, Moll) octave (start, _) = map pitch (map ((absPitch (tone,octave))+) (shift (tone,octave) (start,octave) minorScale))

shift::Pitch->Pitch -> [Int] ->[Int]
shift a b scale = map ((absPitch a) - (absPitch b) -) scale

bassLine :: BassStyle -> [NoteAttribute]->[Pitch]-> [Music]
bassLine Basic vol = basicBassLine vol 0
bassLine Calypso vol = calypsoBassLine vol (-1)
bassLine Boogie vol = boogieBassLine vol 0


basicBassLine ::  [NoteAttribute]-> Int-> [Pitch] -> [Music]
basicBassLine vol 0 m = (Note  (m!!0) hn vol):(basicBassLine vol 4 m)
basicBassLine vol 4 m = (Note  (m!!4) hn vol):(basicBassLine vol 0 m)
basicBassLine vol _ m = []


calypsoBassLine ::  [NoteAttribute]-> Int-> [Pitch] -> [Music]
calypsoBassLine vol (-1) m = (hnr):(boogieBassLine vol 0 m)
calypsoBassLine vol 0 m = (Note (m!!0) qn vol):(boogieBassLine vol 2 m)
calypsoBassLine vol 2 m = (Note (m!!2) qn vol):(boogieBassLine vol (-1) m)
calypsoBassLine vol _ m = []


boogieBassLine ::  [NoteAttribute]-> Int-> [Pitch] -> [Music]
boogieBassLine vol 0 m = (Note (m!!0) qn vol):(Note (m!!4) qn vol):(calypsoBassLine vol 5 m)
boogieBassLine vol 5 m = (Note (m!!5) qn vol):(Note (m!!4) qn vol):(calypsoBassLine vol 0 m)
boogieBassLine vol _ m = []


--autoBass :: BassStyle -> Key -> ChordProgression -> Music

