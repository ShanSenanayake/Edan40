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
\item{\texttt{PitchClass}} is a datatype which represents the twelve basic tones for music.
\item{\texttt{Octave}} is a type that contains an \texttt{Int} which represents which octave the tone belongs to, that means which overtone the original tone is.
\item{\texttt{Pitch}} is a type that contains a tuple of \texttt{(PitchClass,Octave)}. \texttt{Pitch} explains exactly which tone to take.
\item{\texttt{AbsPitch}} is a type that contains an  \texttt{Int} which represents \texttt{Pitch} in a number.
\item{\texttt{Music}} is a datatype which represents music. This type will be exaplained in more detail.
\end{description}

\subsection{Music}
The \texttt{Music} datatype in Haskore is what glues the music in our program together. This datatype is quite complex and it can consist of severa different structures. We will only describe the ones that we have used and they are the following:

\begin{description}
\item{\texttt{Note}} defines a musical note. It is paired with a \texttt{Pitch} and \texttt{Dur} and  a list of \texttt{NoteAttribute}. \texttt{Pitch} defines the exact tone of the note, \texttt{Dur} is the length that the note should be played and it is defined as a number of whole notes. The type of \texttt{Dur} is \texttt{Ratio Int}. \texttt{NoteAttribute} defines some values that defines the notion or musical interpretation that should apply to the note. The only \texttt{NoteAttribute} we have used is to set the volume of the note. For example \texttt{Volume 80}.
\item{\texttt{Rest}} which is paired with a \texttt{Dur} and defines a silent note that should be played as long as \texttt{Dur} specifies.
\item{\texttt{Music :+: Music}} which denotes a sequential composition. This means the \texttt{Music} that is created consists of the first \texttt{Music} object played before the second \texttt{Music} object. For example, if the two \texttt{Music} objects denotes two object of the type \texttt{Note} then the \texttt{Music} object created by the sequential composition is those two notes played after each other.
\item{\texttt{Music :=: Music}} denotes parallell composition. The \texttt{Music} object created by the parallell composition consists of the other two \texttt{Music} notes played in parallell. For example, a chord consists of three notes played in parallell and thus three \texttt{Music} objects of type \texttt{Note} compositioned in parallell.
\item{\texttt{Tempo  (Ratio Int) Music)}} which sets the tempo of the \texttt{Music} object applied. The tempo of the supplied \texttt{Music} object is scaled by a factor that is denoted by the \texttt{Rati Int}.
\item{\texttt{Instr  IName Music}} sets which instrument that the supplied \texttt{Music} object is to be played on. The type of \texttt{IName} is simply \texttt{String}
\end{description}
As stated above there are a few more things that det \texttt{Music} datatype can do but we have only used the ones listed here.
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
> majorScale = [0,2,4,5,7,9,11]
> type ChordProgression = [(PitchClass,Dur)]
> type Chord = [Pitch]
> type Range = (Int,Int)
> type MusicalKey = (PitchClass,Mode)
> type Chordint = [Int]



\end{verbatimtab}
\begin{description}
\item{\texttt{BassStyle}} this type is used along with pattern matching to determine which bassline to play. We were given three different bassline to code therefore we chose to have three different values to our the datatype \texttt{Bassline} namely \texttt{Basic}, \texttt{Calypso} and \texttt{Boogie}.
\item{\texttt{Scale}} is a list of seven \texttt{Pitch} objects which determines the scale of the song beginning on a certain tone (more explaination in the \texttt{generatePitchScale}).
\item{\texttt{majorScale}} this is the orgin scale of the key, this is the only scale we will need and it will be explained more in \texttt{generatePitchScale}.
\item{\texttt{Chord}} is a list of three \texttt{Pitch} objects which determines a Chord.
\item{\texttt{ChordProgresson}} consists of a list of tuples containing \texttt{PitchClass} and \texttt{Dur} 
\item{\texttt{Range}} is a tuple of two \texttt{Int} which gives the range of the chords in \texttt{AbsPitch} value.
\item{}
\end{description}

\section{BassLine}
The first task of our program was to generate three types of bass lines depending on user input. To generate these three bass lines we decided to make three functions which returns infinite lists of the three bass lines.
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
The three bassline functions takes three arguments an \texttt{Int}, a list of \texttt{NoteAttribute} and a \texttt{Scale}. The first argument takes a index of the \texttt{Scale} and makes a music object out of it, if the index is negative then it creates a \texttt{Rest}. The second argument decides the volume of the music object. The third argument says which scale the bassline should play in. \\
To know how long a certain bassline should play in a certain scale we needed an above function which decides how many elements take.
\begin{verbatimtab}

> bassLine :: BassStyle ->Dur -> [NoteAttribute]->Scale-> Music
> bassLine Basic dur vol = line . take (ceiling  (2*  (rtof dur))) . basicBassLine 0 vol
> bassLine Calypso dur vol = line . take (ceiling(8*(rtof dur))) . calypsoBassLine (-1) vol
> bassLine Boogie dur vol =line . take (ceiling(8*(rtof dur))) . boogieBassLine 0 vol

\end{verbatimtab}
The function above takes four arguments. The first argument is used to decide which bassline should be played. The second argument determines for how long a bassline should be played. Depending on the bassline we take different amounts of notes since all of them do not return the same thing. Since \texttt(Dur) is a \texttt{Ratio Int} we need the function \texttt{rtof} which takes a \texttt{Ratio Int} and returns a float, using this we can convert to an \texttt{Int} using the function \texttt{ceiling} so the function \texttt{take} will work. \\ 
The third argument decides the volume of the bassline and finally the fourth argument decides which scale the bassline should be played in.\\
To create a bassline we see that we need a scale, to generate this scale takes us to the next function.



\begin{verbatimtab}

> generatePitchScale :: Key -> Octave -> PitchClass -> Scale
> generatePitchScale key octave start = map pitch (map ((12*octave + key)+) (shift (abs ((pitchClass start) - key)) majorScale))

\end{verbatimtab}
This function takes a \texttt{Key}, which is a Haskore type which represents the \texttt{PitchClass} in an \texttt{Int}, and \texttt{Octave} and a \texttt{PitchClass} and returns a \texttt{Scale}. In the assigment we were given a bunch of different scales to apply depending on where on the scale the tone for a chord was. We decided to disregard most of this since the only thing the "different" scales gave was a shifting of the orginal scale starting on the tone for a certain scale. To obtain this we decided to make a helper function shift.

\begin{verbatimtab}

> shift::Int -> [Int] ->[Int]
> shift n list@(x:xs) 
>	 | n == x = list
>	 | otherwise = shift n (xs++[12+x])


\end{verbatimtab}
The function \texttt{shift} takes the original scale (in every case the \texttt{majorScale} defined above) and shifts it until it hits the current tone. Since the original scale determines how many steps from the origin tone (which is the key) it takes, we had to subtract the origin tone with our new tone to get the difference and then shift the list until it finds it. This gives us a "new" scale which we can apply to the origin tone and get a scale which begins with the new tone. \\
When the shifting is done we take the key in the correct octave inputted above to get a sufficent scale.\\
Using all of the functions above we can combine them and create the \texttt{autoBass} function.

\begin{verbatimtab}

> autoBass :: BassStyle -> Key -> ChordProgression -> Music
> autoBass style key [(c,d)] = (bassLine style d [Volume 50] (generatePitchScale key 3 c))
> autoBass style key ((c,d):prog) = (bassLine style d [Volume 50] (generatePitchScale key 3 c)):+:(autoBass style key prog)

\end{verbatimtab}



\begin{verbatimtab}


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