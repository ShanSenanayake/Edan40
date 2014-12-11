module StringAlignment where
{- The string alignment problem can solve the MCS problem by choosing scoreMatch = 1, and the rest to 0, this will yield a result
 where the string alignment algorithm will maximise the matches giving us the maximal substring of the two-}




type AlignmentType = (String,String)

testString1 = "writers"
testString2 = "vintner"
testString3 = "aferociousmonadatemyhamster"
testString4 = "functionalprogrammingrules"


scoreSpace, scoreMatch, scoreMissmatch :: Int
scoreMatch = 0
scoreSpace = -1
scoreMissmatch = -1

similarityScore :: String -> String -> Int
similarityScore [] [] = 0 
similarityScore [] (y:ys) = (similarityScore [] ys) + (score '-' y)
similarityScore (x:xs) [] = (similarityScore xs []) + (score x '-')
similarityScore string1@(x:xs) string2@(y:ys) = max ((similarityScore xs ys) + (score x y)) (max ((similarityScore string1 ys) + 
	(score '-' y)) ((similarityScore xs string2) + (score x '-')))


score :: Char -> Char -> Int
score x y
	| x == '-' = scoreSpace
	| y == '-' = scoreSpace
	| x == y = scoreMatch
	| otherwise = scoreMissmatch
--For each list in the tuple prepend the two heads
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])] 
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

maximaBy :: Ord b => (a->b) -> [a]-> [a]
maximaBy valueFcn xs = [x | x <-xs, (maximum (map valueFcn xs))==(valueFcn x)]


optAlignments :: String -> String -> [AlignmentType]
optAlignments [] [] = [([],[])]
optAlignments [] (y:ys) = attachHeads '-' y (optAlignments [] ys)
optAlignments (x:xs) [] = attachHeads x '-' (optAlignments xs [])
optAlignments string1@(x:xs) string2@(y:ys) = maximaBy scoring ((attachHeads x  y (optAlignments xs ys))++
	(attachHeads '-'  y (optAlignments string1 ys)) ++ (attachHeads x  '-' (optAlignments xs string2)))



scoring :: (String,String) -> Int
scoring ([],[]) = 0
scoring ([],_) = 0
scoring (_,[]) = 0
scoring ((x:xs),(y:ys)) = score x y + scoring (xs,ys)


outputOptAlignments :: String -> String -> IO ()
outputOptAlignments x y = do
	putStrLn ("There are " ++ (show (length z)) ++ " optimal alignments")
	putLine' z (length z)
		where z = optAlign x y


putLine' :: [(String,String)] -> Int-> IO ()
putLine' [] z = putStrLn ("There are " ++ (show z) ++ " optimal alignments")
putLine' ((x,y):list) z = do
	putStrLn "---------------------"
	putStrLn x
	putStrLn y
	putStrLn "---------------------"
	putLine' list z

