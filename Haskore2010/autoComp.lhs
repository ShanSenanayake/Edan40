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
In this assigment we have constructed a program in Literate Haskell that creates the accompaniment for a given melody and chord progression. The accompaniment consists of two parts, a bass line and a chord voicing. There are three bass lines to choose between, namely, basic bass, calypso bass and boogie bass. The chord voicing automatically generates the "best" version of a chord in the chord progression. The "best" chord is determined by looking at the notes in the triad of the chord and on the chord played previously. \\
We have constructed two other files in Haskell and Haskore format that includes chord progression and melody for two different songs, Twinkle twinkle and Jingle bells. \\
This report will serve as documentation and explanation of the program that we have constructed.

\section{Haskore}
Our program is written as a module called AutoComp and it utilizes Haskore which is a music library for Haskell. Since this assignment is built on Haskore some Haskore datatypes and functionality has to be explained before one can understand our program. \\
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

\section{Musical Theory}
The task given to us is quite limiting on the musical front making us able to do certain shortcuts. One of the main shortcuts we have done in the program is that we do not denote the harmonic quality of the chords, both in the chord progression or in the chords themselves. This can be a bit non intuitive from a readers point of view (or a musician). However since the program itself has no need for the notation of harmonic qualities we chose to do this optimization. 

The reason behind this optimization is that the scale of the song will denote the quality of the chord that fits in the song. For instance if we have a C Major scale in the song (like Twinkle) the tones we can use freely are C, D, E, F, G, A and B.  If we then want the chord D for instance the program will return the tones (D,F,A) which fit in the scale which in this case is a D Minor chord. In similair we will get a G Major if we try the G chord (G,B,D). This makes it fully sufficient to only have the basic tones to denote a chord.

\section{AutoComp}
In this section we we will describe the design and functionality of our program in Literate Haskell format.
\begin{verbatimtab}

> module AutoComp where
> import Haskore
> import Data.Ratio 

\end{verbatimtab}
In the first line of the code above we simply define the source code as our module AutoComp. The second line simply loads Haskore so that we can utilize the library and the third line imports a package needed for working with durations of notes and the type \texttt{Ratio Int}.

\subsection{Types}
We have defined some types in our program in order to make the types of functions more easily read and understandable.
\begin{verbatimtab}

> type BassStyle = [(Int,Dur)]
> type Scale = [Pitch]
> majorScale = [0,2,4,5,7,9,11]
> type ChordProgression = [(PitchClass,Dur)]
> type ChordPatternInPitchClassValue = [Int]
> type Range = (Pitch,Pitch)
> type ChordPattern = [Pitch]
> type MusicalKey = (PitchClass,Mode)

\end{verbatimtab}
\begin{description}
\item{\texttt{BassStyle}} is a type used to determine which bassline to play. The type is a list of tuples which denotes the pattern of a bass line. The first element in these tuples denotes the index of the tone to play in the scale and the second element denotes the duration for which the tone should play. Since a rest can not be represented as an index in the scale we have chosen to denote a rest by the index \texttt{-1}.
\item{\texttt{Scale}} is a list of seven \texttt{Pitch} objects which determines the scale of the song beginning on a certain tone, this will be explained in more detail in the \texttt{generatePitchScale} function.
\item{\texttt{majorScale}} is the origin scale of the key, this is the only scale we will need and it will be explained in more detail in the \texttt{generatePitchScale} function.

\item{\texttt{ChordProgression}} consists of a list of tuples containing \texttt{PitchClass} and \texttt{Dur} which corresponds to the chord and the duration. There is no reason to have Major or Minor on the chord since it will be determined by the scale either way. For example a song that goes in a C major scale has the tones C, D, E, F, G, A, B that defines it. When taking a C chord, the only chord that fits in the scale is a C major chord since the C minor will make the song clash (skära sig). The same is true for a D chord, this will generate a D minor chord which will fit in the scale. This makes it fully sufficient to only have a \texttt{PitchClass} which represents a chord in the \texttt{ChordProgression}.  Since the task given only needs to be able to handle major scale songs (which go in one scale) and only represent Major or Minor chords.
\item{\texttt{ChordPatternInPitchClassValue}} is a list of three \texttt{Int} objects which represents a basic triad of a chord.
\item{\texttt{Range}} is a tuple of two elements of type \texttt{Pitch} objects which define the range of where a chord should be placed.
\item{\texttt{ChordPattern}} is a list of three \texttt{Pitch} objects which determines a chord.
\end{description}


\subsection{BassLine}
The first task of our program was to generate a bass line from a given pattern. We were given three different bass line patterns called Basic, Boogie and Calypso. We have stored these patterns in the variables below. The type of these variables are \texttt{BassStyle}. Since we want to be able to create arbitrarily long bass lines we have chosen to use \texttt{cycle} in order to get the list of bass patterns as infinite lists.



\begin{verbatimtab}

> basic, boogie, calypso :: BassStyle
> basic = cycle [(0,hn),(4,hn)]
> calypso = cycle [(-1,qn),(0,en),(2,en)]
> boogie = cycle [(0,en),(4,en),(5,en),(4,en)]

\end{verbatimtab}

In the code above we have used some macros defined is Haskore. 

\begin{description}
\item[\texttt{hn}] is macro that defines the duration \texttt{Dur} of a half-note.
\item[\texttt{en}]is macro that defines the duration \texttt{Dur} of an eight-note.
\end{description}
To know how long a certain bass line should play in a certain scale we needed an function which decides how many elements take. The function is defined below.
\begin{verbatimtab}

> bassLine :: BassStyle ->Dur -> [NoteAttribute]->Scale-> Music
> bassLine ((i,d):xs) dur vol scale
> 	| (numerator dur) <= 0 = Rest 0
> 	| i == -1 = (Rest d) :+: (bassLine xs (dur-d) vol scale)
> 	| otherwise = (Note (scale!!i) d vol) :+: (bassLine xs (dur-d) vol scale)

\end{verbatimtab}
The function above takes four arguments. The first argument is used to decide which bass line should be played. The second argument determines for how long a bass line should be played and it is a rational number in terms of how many bars that should be played. Depending on the bass line we need to take a different number of elements from their pattern lists. To do this we take elements until the sum of the taken elements equals the total duration of the bass line. If we cannot take elements from the bass line patterns so that the sum of those elements' durations equals the total duration we take one element more so that we get a longer bass line than the song. We do this so that we will have a bass line that plays at least for the entirety of the song. The third argument decides the volume of the bassline and finally the fourth argument decides which scale the bass line should be played in.

As can be seen in the code snippet above, use the index given in the tuples of the bass lines to pick out the right tone to play from the scale. As can also be seen we create a rest if the index is \texttt{-1} and we continue to take notes as long as the remaining total duration is greater than zero.

To create a bass line we see that we need a scale, to generate this scale takes us to the next function that we have defined.



\begin{verbatimtab}

> generatePitchScale :: Key -> Octave -> PitchClass -> Scale
> generatePitchScale key octave start = map pitch (map 
>	((12*octave + key)+) (shift ((pitchClass start) - key) majorScale))

\end{verbatimtab}
This function takes three arguments. The first argument is a \texttt{Key}, which is a Haskore type which represents the \texttt{PitchClass} in an \texttt{Int}. The rest of the arguments are an \texttt{Octave} and a \texttt{PitchClass} and the function returns a \texttt{Scale}. In the assigment we were given a bunch of different scales to apply depending on where on the scale the tone for a chord was. We decided to disregard most of this since the only thing the "different" scales gave was a shifting of the orginal keys scale starting on the tone for a certain scale, the easiest way to explain this is by giving an example. In the Twinkle Twinkle song we have a C Major key, this gives us the scale C, D, E, F, G, A, B. The scales given to us (ie Ionian, Dorian etc) determines which starting point the key scale should be in. For example D will give us the same scale starting in D (D, E, F, G, A, B, C).  To obtain this we decided to make a helper function shift.

\begin{verbatimtab}

> shift::Int -> [Int] ->[Int]
> shift n list@(x:xs)
>	 | n < 0 = shift (n+12) list 
>	 | n == x = list
>	 | otherwise = shift n (xs++[12+x])

\end{verbatimtab}
The function shift takes the tone on which the scale shall start and original scale (in every case the \texttt{majorScale} defined above) and shifts it until it hits the new tone. Since the original scale determines how many steps from the origin tone (which is the key) it takes, we had to subtract the origin tone with our new tone to get the difference and then shift the list until it finds it. This gives us a "new" scale which we can apply to the origin tone and get a scale which begins with the new tone. The first case in the guards \texttt{n < 0 = shift (n+12) list} is needed to ensure that we get the right number which represents the number in the scale. Since we always count from the origin tone and forward this case will ensure that if the origin tone has a larger \texttt{pitchClass} value than the new tone, we get the next overtone of the new tone. \\
When the shifting is done we take the key in the correct octave inputted above to get a sufficient scale. \\
Using all of the functions above we can combine them and create the \texttt{autoBass} function.

\begin{verbatimtab}

> autoBass :: BassStyle -> Key -> ChordProgression -> Music
> autoBass style key cprog = foldl (\acc (c,d) -> 
>	acc :+: (bassLineMusic (scale c) d)) (Rest 0) cprog 
> 		where
> 			scale note = generatePitchScale key 3 note
> 			bassLineMusic theScale dur = 
> 				bassLine style dur [Volume 50] theScale

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
> generateChordRange (low,high) ch = 
>	[pitch c | c <- [(absPitch low)..(absPitch high)] , elem(c `mod` 12) ch]



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
> optimiseLength prev chords = snd $ foldr1 pick 
>	[(scoreChord prev c, c) | c <- chords]
>	where pick a@(sa, _) b@(sb, _)
>		| sa > sb   = b
>		| otherwise = a

> scoreChord:: ChordPattern -> ChordPattern -> Int
> scoreChord prev next = abs  ((sum  (map absPitch prev)) - 
>	(sum  (map absPitch next)))


\end{verbatimtab}
To pick out the "best" \texttt{ChordPattern} out of our list of \texttt{ChordPattern} objects we have to first score them and then evaluate all of them. \\
The scoring is done by the function \texttt{scoreChord} which takes the previous \texttt{ChordPattern} and a potential next \texttt{ChordPattern}  and returns a value for that comparison. The scoring is simple. It just adds all the \texttt{AbsPitch} value of each individual tone in the two comparing \texttt{ChordPattern} objects and then subtracting the sum of the potential next and previous \texttt{ChordPattern}.\\
Given the scoring in list of tuples we fold the list with a function \texttt{pick} which selects the \texttt{ChordPattern}  which has the least score.

\begin{verbatimtab}

> chordToMusic:: (ChordPattern,Dur) -> Music
> chordToMusic ([],d) = Rest 0
> chordToMusic (xs,d) = chord [(Note x d [Volume 50])| x <- xs]


\end{verbatimtab}
The \texttt{chordToMusic} function above takes a tuple of a \texttt{ChordPattern} and \texttt{Dur} and transforms this into the \texttt{Music} type that Haskore has defined. Note that the tuple in the argument has the same type as the elements of a \texttt{ChordProgression}
\begin{verbatimtab}

> generateMusicChord :: Range -> Key -> ChordProgression -> Music
> generateMusicChord range key prog = line $ map chordToMusic $ 
> 	zip (pickBestChords key range chords) durs
> 	where (chords,durs) = unzip prog


> pickBestChords :: Key -> Range -> [PitchClass] -> [ChordPattern]
> pickBestChords key range chords = scanl (optimiseLength) first (tail candidates)
> 	where 
> 		candidates = map (generateCandidates range key) chords 
> 		first = head $ head candidates



> generateCandidates :: Range -> Key -> PitchClass -> [ChordPattern]
> generateCandidates range key c = getChordPatterns 
> 	(generateChordRange range (getBasicTriad key c))

\end{verbatimtab}
The function \texttt{generateMusicChord} calls several functions that we have defined above in order to generate a \texttt{Music} object which will represent the chordline in a song. It is created from a \texttt{Key} and a  \texttt{ChordProgression}. \texttt{generateCandidates}  uses a couple of help functions to do this task, namely \texttt{pickBestChords} which picks the best chords to play from a \texttt{[PitchClass]}, which denotes the list of chords that shall be played, a \texttt{Range} and a \texttt{Key}. To do this it has to first generate all candidates for the chords and then pick the best ones out depending on the previous chord played. \texttt{generateCandidates} takes a \texttt{Range}, \texttt{Key} and a \texttt{PitchClass} and generates all potential chord-candidates for the chord represented by the \texttt{PitchClass} from the given \texttt{Key} in the given \texttt{Range}. 
\begin{verbatimtab}

> autoChord :: Key -> ChordProgression -> Music
> autoChord = generateMusicChord ((E,4),(G,5))

\end{verbatimtab}
The function \texttt{autoChord} is the main function for the chord voicing. It takes a \texttt{Key} and a \texttt{ChordProgression} and generate one \texttt{Music} object that represents all chosen chords in the right order corresponding to the order of the chord progression in a certain \texttt{Range}. 
\begin{verbatimtab}

> mKeyToKey :: MusicalKey -> Key
> mKeyToKey (p,Major) = (pitchClass p)
> mKeyToKey (p,Minor) = ((pitchClass p) + 3) `mod` 12

\end{verbatimtab}
The \texttt{mKeyToKey} function above to transform a certain representation of the key (the assignments's representation) into Haskell's representation of a key. It thus take an argument \texttt{MusicalKey} which is the representation of a key given in the assignment description and transforms it into Haskell's type \texttt{Key}.
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