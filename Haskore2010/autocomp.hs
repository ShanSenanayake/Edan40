module AutoComp where
import Haskore 

data BassStyle = Basic | Calypso | Boogie deriving (Eq)
data Harmony = Dur | Moll deriving (Eq)
type Chord = (PitchClass,Harmony)
type ChordProgression = [(PitchClass,Dur)]

majorScale = [0,2,4,5,7,9,11]

generatePitchScale :: Key -> Octave -> PitchClass -> [Pitch]
generatePitchScale key octave start = map pitch (map ((12*octave + key)+) (shift (abs ((pitchClass start) - key)) majorScale))


shift::Int -> [Int] ->[Int]
shift n list@(x:xs) 
	| n == x = list
	| otherwise = shift n (xs++[12+x])

bassLine :: BassStyle -> [NoteAttribute]->Dur->[Pitch]-> Music
bassLine Basic vol dur p = line  (take (ceiling(2*(rtof dur)))  (basicBassLine vol 0 p))
bassLine Calypso vol dur p = line  (take (ceiling(8*(rtof dur)))  (calypsoBassLine vol (-1) p))
bassLine Boogie vol dur p =line  (take (ceiling(8*(rtof dur)))  (boogieBassLine vol 0 p))


basicBassLine ::  [NoteAttribute]-> Int-> [Pitch] -> [Music]
basicBassLine vol 0 m = (Note  (m!!0) hn vol):(basicBassLine vol 4 m)
basicBassLine vol 4 m = (Note  (m!!4) hn vol):(basicBassLine vol 0 m)
basicBassLine vol _ m = []


calypsoBassLine ::  [NoteAttribute]-> Int-> [Pitch] -> [Music]
calypsoBassLine vol (-1) m = (enr):(enr):(calypsoBassLine vol 0 m)
calypsoBassLine vol 0 m = (Note (m!!0) en vol):(calypsoBassLine vol 2 m)
calypsoBassLine vol 2 m = (Note (m!!2) en vol):(calypsoBassLine vol (-1) m)
calypsoBassLine vol _ m = []


boogieBassLine ::  [NoteAttribute]-> Int-> [Pitch] -> [Music]
boogieBassLine vol 0 m = (Note (m!!0) en vol):(Note (m!!4) en vol):(boogieBassLine vol 5 m)
boogieBassLine vol 5 m = (Note (m!!5) en vol):(Note (m!!4) en vol):(boogieBassLine vol 0 m)
boogieBassLine vol _ m = []


autoBass :: BassStyle -> Key -> ChordProgression -> Music
autoBass style key [(c,d)] = (bassLine style [Volume 80] d (generatePitchScale key 3 c))
autoBass style key ((c,d):prog) = (bassLine style [Volume 80] d (generatePitchScale key 3 c)):+:(autoBass style key prog)
