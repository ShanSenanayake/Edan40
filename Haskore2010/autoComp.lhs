\documentclass[a4paper]{article}
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage[english]{babel}
\usepackage{moreverb}

\title{Assignment 2: Functional Music \\ EDAN40 Functional Programming}
\date{\today}
\author{Fredrik Paulsson \\ dat11fp1@student.lu.se \and Shan Senanayake \\ dat11sse@student.lu.se}
%\setcounter{secnumdepth}{5}
%\setcounter{tocdepth}{5}
\begin{document}
\maketitle
%\tableofcontents

\section{Introduction}
In this assigment we have constructed a program in Literate Haskell that creates the accompaniment for a given melody and chord progression. The accompaniment consists of two parts a bass line and a chord voicing. There are three bass lines to choose between, namely, basic bass, calypso bass and boogie bass. The chord voicing automatically generates the "best" version of a chord in the chord progression. The "best" chord is determined by looking at the notes in the triad of the chord an on the chord played previously.

This report will serve as documentation and explanation of the program that we have constructed.

\section{Haskore}
Our program is written as a module called AutoComp and it utilizes Haskore which is a music library for Haskell. Since this assignment is built on Haskore some Haskore datatypes and functionally has to be explained before one can understand our program. \\
\begin{description}
\item{PitchClass} is a datatype which represents the twelve basic tones for music.
\item{Octave} is a type that contains an \texttt{Int} which represtens which octave the tone belongs to, that means which overtone the original tone is.
\item{Pitch} is a type that contains a tuple of \texttt{(PitchClass,Octave)}. Pitch explains exactly which tone to take.
\item{AbsPitch} is a type that contains an  \texttt{Int} which represents Pitch in a number.
\item{Music} is a datatype which represents music.
\end{description}

\subsection{Music}
The \texttt{Music} datatype in Haskore is what glues the music in our program together. This datatype is quite complex and it can consist of severa different structures. We will only describe the ones that we have used and they are the following:

\begin{itemize}
\item{Note} Which is paired with a \textt{Pitch} and \texttt{Dur} and \texttt{[NoteAttribute]}. \textt{Pitch} defines the exact tone of the note, \texttt{Dur} is the length that the note should be played and it is defined as a number of whole notes. The type of \texttt{Dur} is \texttt{Ratio Int}.

\end{itemize}

The rest of the functions/types will be explained when they show up.
\begin{verbatimtab}

> module AutoComp where
> import Haskore 

\end{verbatimtab}
In the first line of the code above we simply define the source code as our module AutoComp. The second line simply loads Haskore so that we can utilize the library.

\section{Types}
We have defined some types in our program in order to make the types of functions more easily read and understandable.
\begin{verbatimtab}

> data BassStyle = Basic | Calypso | Boogie deriving (Eq)
> type Scale = [Pitch]
> type Chord = [Pitch]
> type Range = (Int,Int)
> type MusicalKey = (PitchClass,Mode)
> type Chordint = [Int]
> type ChordProgression = [(PitchClass,Dur)]
> majorScale = [0,2,4,5,7,9,11]

\end{verbatimtab}
The types will be explained as we encounter them in our program.

\section{BassLine}
The first task of our program was to genereate three types of basslines depending on user input. This is where the datatype \texttt{Bassline} comes into play, this type is used along with pattern matching to determine which bassline to play. We were given three different bassline to code therefore we chose to have three different values to our the datatype \texttt{Bassline} namely \texttt{Basic}, \texttt{Calypso} and \texttt{Boogie}. \\
To generate thease three basslines we decided to make three functions which returns infinite lists of the three basslines so we could take how many beats per bar we needed in the song.
\begin{verbatimtab}


> basicBassLine :: Int->  [NoteAttribute]-> Scale -> [Music]
> basicBassLine 0 vol m = (Note  (m!!0) hn vol):(basicBassLine 4 vol m)
> basicBassLine 4 vol m = (Note  (m!!4) hn vol):(basicBassLine 0 vol m)
> basicBassLine _ vol m = []


> calypsoBassLine ::  Int-> [NoteAttribute]-> Scale -> [Music]
> calypsoBassLine (-1) vol m = (enr):(enr):(calypsoBassLine 0 vol m)
> calypsoBassLine 0 vol m = (Note (m!!0) en vol):(calypsoBassLine 2 vol m)
> calypsoBassLine 2 vol m = (Note (m!!2) en vol):(calypsoBassLine (-1) vol m)
> calypsoBassLine _ vol m = []


> boogieBassLine :: Int->  [NoteAttribute]-> Scale -> [Music]
> boogieBassLine 0 vol m = (Note (m!!0) en vol):(Note (m!!4) en vol):(boogieBassLine 5 vol m)
> boogieBassLine 5 vol m = (Note (m!!5) en vol):(Note (m!!4) en vol):(boogieBassLine 0 vol m)
> boogieBassLine _ vol m = []

\end{verbatimtab}
Looking at the type declaration


\begin{verbatimtab}

> bassLine :: BassStyle ->Dur -> [NoteAttribute]->Scale-> Music
> bassLine Basic dur vol = line . take (ceiling  (2*  (rtof dur))) . basicBassLine 0 vol
> bassLine Calypso dur vol = line . take (ceiling(8*(rtof dur))) . calypsoBassLine (-1) vol
> bassLine Boogie dur vol =line . take (ceiling(8*(rtof dur))) . boogieBassLine 0 vol



> generatePitchScale :: Key -> Octave -> PitchClass -> Scale
> generatePitchScale key octave start = map pitch (map ((12*octave + key)+) (shift (abs ((pitchClass start) - key)) majorScale))


> shift::Int -> [Int] ->[Int]
> shift n list@(x:xs) 
>	 | n == x = list
>	 | otherwise = shift n (xs++[12+x])




> autoBass :: BassStyle -> Key -> ChordProgression -> Music
> autoBass style key [(c,d)] = (bassLine style d [Volume 50] (generatePitchScale key 3 c))
> autoBass style key ((c,d):prog) = (bassLine style d [Volume 50] (generatePitchScale key 3 c)):+:(autoBass style key prog)



> getChords :: Chord-> [Chord]
> getChords list 
>	 | (length list) >= 3 = (take 3 list):(getChords (tail list))
>	 | otherwise = []

> getBasicTriad :: Key -> PitchClass -> Chordint
> getBasicTriad key pitch= [pitchClass (fst (scale!!0)),pitchClass (fst (scale!!2)),pitchClass (fst (scale!!4))]
>	 where scale = generatePitchScale key 4 pitch

> generateChordRange :: Range -> Chordint  -> Int -> Chord
> generateChordRange range@(low,high) ch itr 
>	 | itr<low = generateChordRange range ch (itr+1)
>	 | low<= itr && itr <= high = (checkPitch ch itr)++(generateChordRange range ch (itr+1))
>	 | otherwise = []

> checkPitch :: Chordint ->Int->Chord
> checkPitch list itr 
>	 | elem (itr `mod` 12) list = [pitch itr]
>	 | otherwise = []

> optimiseLength :: Chord -> [Chord] -> Chord
> optimiseLength prev chords =  snd (iterateDiff (zip (scoreChord prev chords) chords))


> iterateDiff:: [(Int,Chord)] -> (Int,Chord)
> iterateDiff [(score,ch)] = (score,ch)
> iterateDiff (x:xs) = try x (iterateDiff xs)


> scoreChord:: Chord -> [Chord] -> [Int]
> scoreChord prev chords = [abs  ((sum  (map absPitch prev)) - (sum  (map absPitch next))) | next <- chords]

> try :: (Int,Chord) -> (Int,Chord) -> (Int,Chord)
> try first@(a,b) second@(c,d)
>	 | a>c = second
>	 | otherwise = first

> chordToMusic:: (Chord,Dur) -> Music
> chordToMusic ([],d) = Rest 0
> chordToMusic ((x:xs),d) = (Note x d [Volume 50]):=:(chordToMusic (xs,d))


> generateMusicChord :: Key -> ChordProgression -> Chord -> [Music]
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