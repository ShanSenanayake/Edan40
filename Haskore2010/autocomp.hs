module AutoComp where
import Haskore 

data BassStyle = Basic | Calypso | Boogie deriving (Eq)
data Harmony = Dur | Moll deriving (Eq)
type Chord = (Int,Int,Int)
type ChordProgression = [(PitchClass,Dur)]

majorScale = [0,2,4,5,7,9,11]
chordRange = [(E,4),(F,4),(G,4),(A,4),(B,4),(C,5),(D,5),(E,5),(F,5),(G,5)]

generatePitchScale :: Key -> Octave -> PitchClass -> [Pitch]
generatePitchScale key octave start = map pitch (map ((12*octave + key)+) (shift (abs ((pitchClass start) - key)) majorScale))


shift::Int -> [Int] ->[Int]
shift n list@(x:xs) 
	| n == x = list
	| otherwise = shift n (xs++[12+x])

bassLine :: BassStyle ->Dur -> [NoteAttribute]->[Pitch]-> Music
bassLine Basic dur vol = line . take (ceiling  (2*  (rtof dur))) . basicBassLine 0 vol
bassLine Calypso dur vol = line . take (ceiling(8*(rtof dur))) . calypsoBassLine (-1) vol
bassLine Boogie dur vol =line . take (ceiling(8*(rtof dur))) . boogieBassLine 0 vol


basicBassLine :: Int->  [NoteAttribute]-> [Pitch] -> [Music]
basicBassLine 0 vol m = (Note  (m!!0) hn vol):(basicBassLine 4 vol m)
basicBassLine 4 vol m = (Note  (m!!4) hn vol):(basicBassLine 0 vol m)
basicBassLine _ vol m = []


calypsoBassLine ::  Int-> [NoteAttribute]-> [Pitch] -> [Music]
calypsoBassLine (-1) vol m = (enr):(enr):(calypsoBassLine 0 vol m)
calypsoBassLine 0 vol m = (Note (m!!0) en vol):(calypsoBassLine 2 vol m)
calypsoBassLine 2 vol m = (Note (m!!2) en vol):(calypsoBassLine (-1) vol m)
calypsoBassLine _ vol m = []


boogieBassLine :: Int->  [NoteAttribute]-> [Pitch] -> [Music]
boogieBassLine 0 vol m = (Note (m!!0) en vol):(Note (m!!4) en vol):(boogieBassLine 5 vol m)
boogieBassLine 5 vol m = (Note (m!!5) en vol):(Note (m!!4) en vol):(boogieBassLine 0 vol m)
boogieBassLine _ vol m = []


autoBass :: BassStyle -> Key -> ChordProgression -> Music
autoBass style key [(c,d)] = (bassLine style d [Volume 80] (generatePitchScale key 3 c))
autoBass style key ((c,d):prog) = (bassLine style d [Volume 80] (generatePitchScale key 3 c)):+:(autoBass style key prog)


--autoChord :: Key -> ChordProgression -> Music

getChord :: Key -> PitchClass -> Chord
getChord key pitch= (pitchClass (fst (scale!!0)),pitchClass (fst (scale!!2)),pitchClass (fst (scale!!4)))
	where scale = generatePitchScale key 4 pitch


--optimiseChord :: Chord -> Chord -> Music
