\documentclass[a4paper]{article}
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage[english]{babel}
\usepackage{moreverb}

\title{Functional Music \\ EDAN40 Functional Programming}
\date{\today}
\author{Fredrik Paulsson \\ dat11fp1@student.lu.se \and Shan Senanayake \\ dat11sse@student.lu.se}
%\setcounter{secnumdepth}{5}
%\setcounter{tocdepth}{5}
\begin{document}
\maketitle
%\tableofcontents

\begin{verbatimtab}

> module AutoComp where
> import Haskore 

> data BassStyle = Basic | Calypso | Boogie deriving (Eq)
> type MusicalKey = (PitchClass,Mode)
> type Chord = [Int]
> type ChordProgression = [(PitchClass,Dur)]

> majorScale = [0,2,4,5,7,9,11]

> generatePitchScale :: Key -> Octave -> PitchClass -> [Pitch]
> generatePitchScale key octave start = map pitch (map ((12*octave + key)+) (shift (abs ((pitchClass start) - key)) majorScale))


> shift::Int -> [Int] ->[Int]
> shift n list@(x:xs) 
>	 | n == x = list
>	 | otherwise = shift n (xs++[12+x])

> bassLine :: BassStyle ->Dur -> [NoteAttribute]->[Pitch]-> Music
> bassLine Basic dur vol = line . take (ceiling  (2*  (rtof dur))) . basicBassLine 0 vol
> bassLine Calypso dur vol = line . take (ceiling(8*(rtof dur))) . calypsoBassLine (-1) vol
> bassLine Boogie dur vol =line . take (ceiling(8*(rtof dur))) . boogieBassLine 0 vol


> basicBassLine :: Int->  [NoteAttribute]-> [Pitch] -> [Music]
> basicBassLine 0 vol m = (Note  (m!!0) hn vol):(basicBassLine 4 vol m)
> basicBassLine 4 vol m = (Note  (m!!4) hn vol):(basicBassLine 0 vol m)
> basicBassLine _ vol m = []


> calypsoBassLine ::  Int-> [NoteAttribute]-> [Pitch] -> [Music]
> calypsoBassLine (-1) vol m = (enr):(enr):(calypsoBassLine 0 vol m)
> calypsoBassLine 0 vol m = (Note (m!!0) en vol):(calypsoBassLine 2 vol m)
> calypsoBassLine 2 vol m = (Note (m!!2) en vol):(calypsoBassLine (-1) vol m)
> calypsoBassLine _ vol m = []


> boogieBassLine :: Int->  [NoteAttribute]-> [Pitch] -> [Music]
> boogieBassLine 0 vol m = (Note (m!!0) en vol):(Note (m!!4) en vol):(boogieBassLine 5 vol m)
> boogieBassLine 5 vol m = (Note (m!!5) en vol):(Note (m!!4) en vol):(boogieBassLine 0 vol m)
> boogieBassLine _ vol m = []


> autoBass :: BassStyle -> Key -> ChordProgression -> Music
> autoBass style key [(c,d)] = (bassLine style d [Volume 50] (generatePitchScale key 3 c))
> autoBass style key ((c,d):prog) = (bassLine style d [Volume 50] (generatePitchScale key 3 c)):+:(autoBass style key prog)



> getChords :: [Pitch]-> [[Pitch]]
> getChords list 
>	 | (length list) >= 3 = (take 3 list):(getChords (tail list))
>	 | otherwise = []

> getBasicTriad :: Key -> PitchClass -> Chord
> getBasicTriad key pitch= [pitchClass (fst (scale!!0)),pitchClass (fst (scale!!2)),pitchClass (fst (scale!!4))]
>	 where scale = generatePitchScale key 4 pitch

> generateChordRange :: (Int,Int) ->Chord -> Int -> [Pitch]
> generateChordRange range@(low,high) ch itr 
>	 | itr<low = generateChordRange range ch (itr+1)
>	 | low<= itr && itr <= high = (checkPitch ch itr)++(generateChordRange range ch (itr+1))
>	 | otherwise = []

> checkPitch :: Chord->Int->[Pitch]
> checkPitch list itr 
>	 | elem (itr `mod` 12) list = [pitch itr]
>	 | otherwise = []

> optimiseLength :: [Pitch] -> [[Pitch]] -> [Pitch]
> optimiseLength prev chords =  snd (iterateDiff (zip (scoreChord prev chords) chords))


> iterateDiff:: [(Int,[Pitch])] -> (Int,[Pitch])
> iterateDiff [(score,ch)] = (score,ch)
> iterateDiff (x:xs) = try x (iterateDiff xs)


> scoreChord:: [Pitch] -> [[Pitch]] -> [Int]
> scoreChord prev chords = [abs  ((sum  (map absPitch prev)) - (sum  (map absPitch next))) | next <- chords]

> try :: (Int,[Pitch]) -> (Int,[Pitch]) -> (Int,[Pitch])
> try first@(a,b) second@(c,d)
>	 | a>c = second
>	 | otherwise = first

> chordToMusic:: ([Pitch],Dur) -> Music
> chordToMusic ([],d) = Rest 0
> chordToMusic ((x:xs),d) = (Note x d [Volume 50]):=:(chordToMusic (xs,d))


> generateMusicChord :: Key -> ChordProgression -> [Pitch] -> [Music]
> generateMusicChord key [(c,d)] prev = [chordToMusic(optimiseLength prev (getChords (generateChordRange (52,67) (getBasicTriad key c) 0)),d)]
> generateMusicChord key ((c,d):prog) prev = (chordToMusic(next):(generateMusicChord key prog (fst next)))
>	 where next = (optimiseLength prev (getChords (generateChordRange (52,67) (getBasicTriad key c) 0)),d)

> autoChord :: Key -> ChordProgression -> Music
> autoChord key ((c,d):prog) = line ((chordToMusic(first)):(generateMusicChord key prog (fst first)))
>	 where first = ((head (getChords (generateChordRange (52,67) (getBasicTriad key c) 0))),d)

> mKeyToKey :: MusicalKey -> Key
> mKeyToKey (p,Major) = (pitchClass p)
> mKeyToKey (p,Minor) = ((pitchClass p) + 3) `mod` 12

> autoComp :: BassStyle -> MusicalKey -> ChordProgression->Music
> autoComp style mKey progression = (autoBass style (mKeyToKey mKey) progression):=:(autoChord (mKeyToKey mKey) progression)

\end{verbatimtab}


%\begin{thebibliography}{1}
%\bibitem{wikipedia}
%http://en.wikipedia.org
%\end{thebibliography}
\end{document}