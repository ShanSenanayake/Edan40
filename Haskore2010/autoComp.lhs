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
In this assigment we have constructed a program in Literate Haskell that creates the accompaniment for a given melody and chord progression. The accompaniment consists of two parts a bass line and a chord voicing. There are three bass lines to choose between, namely, basic bass, calypso bass and boogie bass. The chord voicing automatically generates the "best" version of a chord in the chord progression. The "best" chord is determined by looking at the notes in the triad of the chord an on the chord played previously. \\
We have constructed two other files in Haskell and Haskore format that includes chord progression and melody for two different songs, Twinkle twinkle and Jingle bells. \\
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
The \texttt{Music} datatype in Haskore is what glues the music in our program together. This datatype is quite complex and it can consist of several different structures. We will only describe the ones that we have used and they are the following:

\begin{description}
\item{\texttt{Note}} defines a musical note. It is paired with a \texttt{Pitch} and \texttt{Dur} and  a list of \texttt{NoteAttribute}. \texttt{Pitch} defines the exact tone of the note, \texttt{Dur} is the length that the note should be played and it is defined as a rational number of whole notes. The type of \texttt{Dur} is \texttt{Ratio Int}. \texttt{NoteAttribute} defines some values that defines the notion or musical interpretation that should apply to the note. The only \texttt{NoteAttribute} we have used is to set the volume of the note. For example \texttt{Volume 80}.
\item{\texttt{Rest}} which is paired with a \texttt{Dur} and defines a silent note that should be played as long as \texttt{Dur} specifies.
\item{\texttt{Music :+: Music}} which denotes a sequential composition. This means the \texttt{Music} that is created consists of the first \texttt{Music} object played before the second \texttt{Music} object. For example, if the two \texttt{Music} objects to be compositioned denotes two objects of the type \texttt{Note} then the \texttt{Music} object created by the sequential composition is those two notes played after each other.
\item{\texttt{Music :=: Music}} denotes parallell composition. The \texttt{Music} object created by the parallell composition consists of the other two \texttt{Music} notes played in parallell. For example, a chord consists of three notes played in parallell and thus three \texttt{Music} objects of type \texttt{Note} compositioned in parallell.
\item{\texttt{Tempo  (Ratio Int) Music}} which sets the tempo of the \texttt{Music} object applied. The tempo of the supplied \texttt{Music} object is scaled by a factor that is denoted by the rational number \texttt{Ratio Int}.
\item{\texttt{Instr  IName Music}} sets which instrument that the supplied \texttt{Music} object is to be played on. The type of \texttt{IName} is simply \texttt{String}. It can take values such as \texttt{"piano"}, \texttt{"guitar"} and several others.
\end{description}
As stated above there are a few more things that the \texttt{Music} datatype can do but we have only used the ones listed here.

\section{AutoComp}
In this section we we will describe the design and functionality of our program in Literate Haskell format.
\begin{verbatimtab}

> module AutoComp where
> import Haskore 

\end{verbatimtab}
In the first line of the code above we simply define the source code as our module AutoComp. The second line simply loads Haskore so that we can utilize the library.

\subsection{Types}
We have defined some types in our program in order to make the types of functions more easily read and understandable.
\begin{verbatimtab}

> data BassStyle = Basic | Calypso | Boogie deriving (Eq)
> type Scale = [Pitch]
> majorScale = [0,2,4,5,7,9,11]
> type ChordProgression = [(PitchClass,Dur)]
> type ChordPatternInPitchClassValue = [Int]
> type Range = (AbsPitch,AbsPitch)
> type ChordPattern = [Pitch]
> type MusicalKey = (PitchClass,Mode)

\end{verbatimtab}
\begin{description}
\item{\texttt{BassStyle}} is a type used along with pattern matching to determine which bassline to play. We were given three different bassline to implement and therefore we chose to have three different values to our the datatype \texttt{Bassline} namely \texttt{Basic}, \texttt{Calypso} and \texttt{Boogie}.
\item{\texttt{Scale}} is a list of seven \texttt{Pitch} objects which determines the scale of the song beginning on a certain tone, this will be explained in more detail in the \texttt{generatePitchScale}.
\item{\texttt{majorScale}} is the origin scale of the key, this is the only scale we will need and it will be explained in more detail in \texttt{generatePitchScale}.

\item{\texttt{ChordProgression}} consists of a list of tuples containing \texttt{PitchClass} and \texttt{Dur} which corresponds to the chord and the duration. There is no reason to have Major or Minor on the chord since it will be determined by the scale either way. For example a song that goes in a C major scale has the tones C, D, E, F, G, A, B that defines it. When taking a C chord, the only chord that fits in the scale is a C major chord since the C minor will make the song clash (skära sig). The same is true for a D chord, this will generate a D minor chord which will fit in the scale. This makes it fully sufficient to only have a \texttt{PitchClass} which represents a chord in the \texttt{ChordProgressIon}.  Since the task given only needs to be able to handle major scale songs (which go in one scale) and only represent Major or Minor chords.
\item{\texttt{ChordPatternInPitchClassValue}} is a list of three \texttt{Int} objects which represents a basic triad of a chord.
\item{\texttt{Range}} is a tuple of two elements of type \texttt{AbsPitch} objects which define the range of where a chord should be placed.
\item{\texttt{ChordPattern}} is a list of three \texttt{Pitch} objects which determines a chord.

\item{\texttt{Range}} is a tuple of two elements of type \texttt{Int} which gives the range of the chords in \texttt{AbsPitch} value.
\item{}
\end{description}

\subsection{BassLine}
The first task of our program was to generate three types of bass lines depending on user input. To generate these bass lines we decided to make three functions which returns infinite lists of the three bass lines.
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
> boogieBassLine 0 vol m = (Note (m!!0) en vol):(Note (m!!4) en vol):
> 	(boogieBassLine 5 vol m)
> boogieBassLine 5 vol m = (Note (m!!5) en vol):(Note (m!!4) en vol):
> 	(boogieBassLine 0 vol m)
> boogieBassLine _ vol m = []

\end{verbatimtab}
The three bass line functions takes three arguments an \texttt{Int}, a list of \texttt{NoteAttribute} and a \texttt{Scale}. The first argument is an index used in the \texttt{Scale} and makes a music object out of it, if the index is negative then it creates a \texttt{Rest}. If the index is not negative it selects the tone that should be played from the available tones defined by the scale. The second argument decides the volume of the music object. The third argument the scale in which the bass line should play in.\\
In the code snipped above we have used some macros defined is Haskore. 

\begin{description}
\item[\texttt{hn}] is macro that defines the duration \texttt{Dur} of a half-note.
\item[\texttt{enr}] is macro that defines an eight-note rest.
\item[\texttt{en}]is macro that defines the duration \texttt{Dur} of an eight-note.
\end{description}
To know how long a certain bass line should play in a certain scale we needed an function which decides how many elements take. The function is defined below.
\begin{verbatimtab}

> bassLine :: BassStyle ->Dur -> [NoteAttribute]->Scale-> Music
> bassLine Basic dur vol = line . take (ceiling  (2*(rtof dur))) 
>	. basicBassLine 0 vol
> bassLine Calypso dur vol = line . take (ceiling(8*(rtof dur))) 
>	. calypsoBassLine (-1) vol
> bassLine Boogie dur vol = line . take (ceiling(8*(rtof dur))) 
>	. boogieBassLine 0 vol

\end{verbatimtab}
The function above takes four arguments. The first argument is used to decide which bass line should be played. The second argument determines for how long a bass line should be played and it is a rational number in terms of how many bars that should be played. Depending on the bass line we take different amounts of notes since all of them do not return the same thing. Since \texttt{Dur} is a \texttt{Ratio Int} we need the function \texttt{rtof} which takes a \texttt{Ratio Int} and returns a float, using this we can convert to an \texttt{Int} using the function \texttt{ceiling} so the function \texttt{take} will work. The third argument decides the volume of the bassline and finally the fourth argument decides which scale the bass line should be played in.\\
To create a bass line we see that we need a scale, to generate this scale takes us to the next function that we have defined.



\begin{verbatimtab}

> generatePitchScale :: Key -> Octave -> PitchClass -> Scale
> generatePitchScale key octave start = map pitch (map 
>	((12*octave + key)+) (shift ((pitchClass start) - key) majorScale))

\end{verbatimtab}
This function takes three arguments. The first argument is a \texttt{Key}, which is a Haskore type which represents the \texttt{PitchClass} in an \texttt{Int}. The rest of the arguments are an \texttt{Octave} and a \texttt{PitchClass} and the function returns a \texttt{Scale}. In the assigment we were given a bunch of different scales to apply depending on where on the scale the tone for a chord was. We decided to disregard most of this since the only thing the "different" scales gave was a shifting of the orginal keys scale starting on the tone for a certain scale, the easiest way to explain this i by giving an example. In the Twinkle Twinkle song we have a C Major key, this gives us the scale C, D, E, F, G, A, B. The scales given to us (ie Ionian, Dorian etc) determines which starting point the key scale should be in. For example D will give us the same scale starting in D (D, E, F, G, A, B, C).  To obtain this we decided to make a helper function shift.

\begin{verbatimtab}

> shift::Int -> [Int] ->[Int]
> shift n list@(x:xs)
>	 | n < 0 = shift (n+12) list 
>	 | n == x = list
>	 | otherwise = shift n (xs++[12+x])

\end{verbatimtab}
The function shift takes the original scale (in every case the \texttt{majorScale} defined above) and shifts it until it hits the new tone. Since the original scale determines how many steps from the origin tone (which is the key) it takes, we had to subtract the origin tone with our new tone to get the difference and then shift the list until it finds it. This gives us a "new" scale which we can apply to the origin tone and get a scale which begins with the new tone. The first case in the guards \texttt{n < 0 = shift (n+12) list} is needed to ensure that we get the right number which represents the number in the scale. Since we always count from the origin tone and forward this case will ensure that if the origin tone has a larger \texttt{pitchClass} value than the new tone, we get the next overtone of the new tone. \\
When the shifting is done we take the key in the correct octave inputted above to get a sufficient scale. \\
Using all of the functions above we can combine them and create the \texttt{autoBass} function.

\begin{verbatimtab}

> autoBass :: BassStyle -> Key -> ChordProgression -> Music
> autoBass style key cprog = foldl (\acc (c,d) -> acc :+: ((bassLine style d [Volume 50] 
>	(generatePitchScale key 3 c)))) (Rest 0) cprog 

\end{verbatimtab}
The \texttt{autoBass} function is the main function of the bass lines. It takes all the functions above and applies it to the \texttt{Chordprogression} for each individual "chord" and then combines it all into \texttt{Music}.


\subsection{Chord Voicing}
The second part of our program was to generate a chord voicing to the given chord progression. Since a chord consists of three different tones and we had to choose the best one according to a set of rules it made the task a bit more challenging. To abide by all the rules as much as possible we have a bunch of subfunctions.
\begin{verbatimtab}

> getBasicTriad :: Key -> PitchClass -> ChordPatternInPitchClassValue
> getBasicTriad key pitch = map (pitchClass.fst) [scale!!0,scale!!2,scale!!4]
>	 where scale = generatePitchScale key 4 pitch


\end{verbatimtab}
This function gets the three basic tones for a given chord in a key. We utilize the function \texttt{generatePitchScale} to get the  original scale starting at the base tone for a chord and then taking on the intervall (0,2,4) from the scale to get the chord. Since we only want the naive triad we take the \texttt{PitchClass} out of the \texttt{Pitch} and utilize a function from Haskcore called \texttt{pitchClass} which returns an \texttt{Int} from a \texttt{PitchClass} and thus giving us a \texttt{ChordPatternInPitchClassValue}.

\begin{verbatimtab}

> generateChordRange :: Range -> ChordPatternInPitchClassValue  -> [Pitch]
> generateChordRange (low,high) ch = [pitch c | c <- [low..high] , elem(c `mod` 12) ch]



\end{verbatimtab}
The function \texttt{generateChordRange} takes a \texttt{Range} and a  \texttt{ChordPatternInPitchClassValue} for which it produces all the tones in a chord which fit into the given range in order of lowest tone to highest tone.
When the function \texttt{generateChordRange} is done we can utilize this function to only take out the "tightest" chords in the range. This is done to get a good estimate for the "best" chord. 
\begin{verbatimtab}

> getChordPatterns :: [Pitch]-> [ChordPattern]
> getChordPatterns list 
>	 | (length list) >= 3 = (take 3 list):(getChordPatterns (tail list))
>	 | otherwise = []

\end{verbatimtab}
The function \texttt{getChordPatterns} takes the range of \texttt{Pitch} objects which define the chord and returns a list of \texttt{ChordPattern} objects which are the "tightest" chords in the range. The way this works is by taking the lowest three tones and using that as one possibility for the chord and then throw away the lowest tone. Once again it selects the new lowest three tones and add that to the list of possible chords. This is repeated until there are only two tones left in the range. This yields a list of possible chords that are all reasonably tight.\\
Now that we have a bunch of \texttt{ChordPattern} objects to compare we can start to pick out the "best" one. To pick out the "best" chord we have to compare with the previously played chord.
\begin{verbatimtab}

> optimiseLength :: ChordPattern -> [ChordPattern] -> ChordPattern
> optimiseLength prev chords =  snd (iterateDiff 
>	(zip (scoreChord prev chords) chords))

> scoreChord:: ChordPattern -> [ChordPattern] -> [Int]
> scoreChord prev chords = [abs  ((sum  (map absPitch prev)) - 
>	(sum  (map absPitch next))) | next <- chords]

> iterateDiff:: [(Int,ChordPattern)] -> (Int,ChordPattern)
> iterateDiff [(score,ch)] = (score,ch)
> iterateDiff (x:xs) = evaluateScore x (iterateDiff xs)

> evaluateScore :: (Int,ChordPattern) -> (Int,ChordPattern) -> (Int,ChordPattern)
> evaluateScore first@(a,b) second@(c,d)
>	 | a>c = second
>	 | otherwise = first

\end{verbatimtab}
To pick out the "best" \texttt{ChordPattern} out of our list of \texttt{ChordPattern} objects we have to first score them and then evaluate all of them. \\
The scoring is done by the function \texttt{scoreChord} which takes the previous \texttt{ChordPattern} and the list of \texttt{ChordPattern} objects sutible for playing next, and returns a list of \texttt{Int} which has the score. The scoring is simple. It just adds all the \texttt{AbsPitch} value of each individual tone in the two comparing \texttt{ChordPattern} objects and then subtracting the sum of the potential next and previous \texttt{ChordPattern}.\\
Given the score we \texttt{zip} the two lists to map the score to the respective chord. Using this list of tuples in the function \texttt{IterateDiff} we take each tuple and evaluate the score using function \texttt{evaluateScore}. The \texttt{evaluateScore} function gives us the smallest tuple of the two, this leads \texttt{IterateDiff} to return the tuple with the least score.\\
The function \texttt{optimiseLength} takes in the previous \texttt{ChordPattern} and the list of potential next \texttt{ChordPattern} objects and using all the functions mentioned above returns the least scored \texttt{ChordPattern} which will be the next \texttt{ChordPattern} played.

\begin{verbatimtab}

> chordToMusic:: (ChordPattern,Dur) -> Music
> chordToMusic ([],d) = Rest 0
> chordToMusic ((x:xs),d) = (Note x d [Volume 50]):=:(chordToMusic (xs,d))

\end{verbatimtab}
The \texttt{chordToMusic} function above takes a tuple of a \texttt{ChordPattern} and \texttt{Dur} and transforms this into the \texttt{Music} type that Haskore has defined. Note that the tuple in the argument has the same type as the elements of a \texttt{ChordProgression}
\begin{verbatimtab}

> generateMusicChord :: Key -> ChordProgression -> ChordPattern -> [Music]
> generateMusicChord key [(c,d)] prev = [chordToMusic(optimiseLength 
>	prev (getChordPatterns (generateChordRange (52,67) (getBasicTriad key c))),d)]
> generateMusicChord key ((c,d):prog) prev = (chordToMusic(next):
>	(generateMusicChord key prog (fst next)))
>	 where next = (optimiseLength prev (getChordPatterns (generateChordRange (52,67) 
>				(getBasicTriad key c))),d)

\end{verbatimtab}
The function \texttt{generateMusicChord} calls several functions that we have defined above in order to generate a list of \texttt{Music} from a \texttt{Key}, \texttt{ChordProgression} and a \texttt{ChordPattern} which denotes chord played right before the music generated by this function. The list of \texttt{Music} types that the function returns are the selected chords that should be played. The list is sorted in the meaning that the order of the chord progression are represented in the order of the list of \texttt{Music} objects. The reason for having an input corresponding to a previously played chord is because the function is recursive and in every step the chord represented in the \texttt{Music} object created are directly dependent on the chord generated in the previous iteration. The problem we encountered here was what to send as previous chord when the function is called for the first time. However we solved this problem in the next function defined.
\begin{verbatimtab}

> autoChord :: Key -> ChordProgression -> Music
> autoChord key ((c,d):prog) = line ((chordToMusic(first)):
>	(generateMusicChord key prog (fst first)))
>	 where first = ((head (getChordPatterns (generateChordRange (52,67) 
>				(getBasicTriad key c)))),d)

\end{verbatimtab}
The function \texttt{autoChord} is the main function for the chord voicing. It takes a \texttt{Key} and a \texttt{ChordProgression} and generate one \texttt{Music} object that represents all chosen chords in the right order corresponding to the order of the chord progression. In order to create one big \texttt{Music} object we used the function \texttt{line} defined in Haskell which simply takes a list of \texttt{Music} objects (sorted in the order they are to be played) and composition them in sequence. Here we also solved the problem of what to choose as previous chord when generating the first "best" chord. Our solution was to take the first tightest chord and use that as the previous chord in the beginning.
\begin{verbatimtab}

> mKeyToKey :: MusicalKey -> Key
> mKeyToKey (p,Major) = (pitchClass p)
> mKeyToKey (p,Minor) = ((pitchClass p) + 3) `mod` 12

\end{verbatimtab}
The \texttt{mKeyToKey} function above to transform a certain representation of the key into Haskell's representation of a key. It thus take an argument \texttt{MusicalKey} which is the representation of a key given in the assignment description and transforms it into Haskell's type \texttt{Key}.
\begin{verbatimtab}

> autoComp :: BassStyle -> MusicalKey -> ChordProgression->Music
> autoComp style mKey progression = (autoBass style (mKeyToKey mKey) 
>	progression):=:(autoChord (mKeyToKey mKey) progression)

\end{verbatimtab}

\texttt{autoComp} is the final and also main function of the entire program. It takes three arguments, \texttt{BassStyle}, \texttt{MusicalKey} and a \texttt{ChordProgression} from which it returns one \texttt{Music} object which consists of the bass line and the chord voicing compositioned in parallell. It thus call the two main functions for bass lines and chord voicing, \texttt{autoBass} and \texttt{autoChord} and composition the results in parallell. It also utilizes the \texttt{mKeyToKey} function in order to transform the representation of the key.


%\begin{thebibliography}{1}
%\bibitem{wikipedia}
%http://en.wikipedia.org
%\end{thebibliography}
\end{document}