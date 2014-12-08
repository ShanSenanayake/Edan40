module StringAlignment where


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
similarityScore string1@(x:xs) string2@(y:ys) = max ((similarityScore xs ys) + (score x y)) (max ((similarityScore string1 ys) + (score '-' y)) ((similarityScore xs string2) + (score x '-')))


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
optAlignments string1@(x:xs) string2@(y:ys) = maximaBy scoring ((attachHeads x  y (optAlignments xs ys))++(attachHeads '-'  y (optAlignments string1 ys)) ++ (attachHeads x  '-' (optAlignments xs string2)))



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

simScore :: String -> String -> Int
simScore xs ys = simLen (length xs) (length ys)
  where
    simLen i j = simTable!!i!!j
    simTable = [[ simEntry i j | j<-[0..]] | i<-[0..] ]
       
    simEntry :: Int -> Int -> Int
    simEntry 0 0 = 0
    simEntry i 0 = (i-1)*scoreSpace
    simEntry 0 j = (j-1)*scoreSpace
    simEntry i j = max ((simLen (i-1) (j-1)) + (score x y)) (max (simLen i (j-1) + scoreSpace) (simLen (i-1) j) + scoreSpace)
      where
         x = xs!!(i-1)
         y = ys!!(j-1)

optAlign :: String -> String -> [AlignmentType]
optAlign xs ys = snd (optLen (length xs) (length ys))
	where
		optLen i j = optTable!!i!!j
		optTable = [[optEntry i j | j <-[0..]] | i <- [0..]]

		optEntry :: Int -> Int -> (Int, [AlignmentType])
		optEntry 0 0 = (0,[([],[])])
		optEntry i 0 = (scoreSpace + fst (optLen (i-1) 0),(attachHeads (xs!!((length xs)-i)) '-' (snd (optLen (i-1) 0))) ) 
		optEntry 0 j = (scoreSpace + fst (optLen 0 (j-1)),(attachHeads '-' (ys!!((length ys)-j)) (snd (optLen 0 (j-1)))) ) 
		optEntry i j 
			| a == b && a == c = (a,(attachHeads x y (snd (optLen (i-1) (j-1))))++(attachHeads x '-' (snd (optLen (i-1) j)))++(attachHeads '-' y (snd (optLen i (j-1)))))
			| a == b = max'(a,(attachHeads x y (snd (optLen (i-1) (j-1))))++(attachHeads x '-' (snd (optLen (i-1) j)))) (c,(attachHeads '-' y (snd (optLen i (j-1)))))
			| a == c = max'(a,(attachHeads x y (snd (optLen (i-1) (j-1))))++(attachHeads '-' y (snd (optLen i (j-1))))) (b,(attachHeads x '-' (snd (optLen (i-1) j))))
			| b == c = max'(a,(attachHeads x y (snd (optLen (i-1) (j-1))))) (b,(attachHeads x '-' (snd (optLen (i-1) j)))++(attachHeads '-' y (snd (optLen i (j-1)))))
			| otherwise = max'(a,(attachHeads x y (snd (optLen (i-1) (j-1))))) (max' (b,(attachHeads x '-' (snd (optLen (i-1) j)))) (c,(attachHeads '-' y (snd (optLen i (j-1))))))
				where
					a = (fst (optLen (i-1) (j-1))) + score x y
					b = (fst (optLen (i-1) j)) + scoreSpace
					c = (fst (optLen i (j-1))) + scoreSpace
					x = xs!!((length xs)-i)
					y = ys!!((length ys)-j)


max' :: (Int,a) -> (Int,a) -> (Int,a)
max' first@(a1,a2) second@(b1,b2) 
	| a1>b1 = first
	| otherwise = second



optAligns :: String -> String -> [AlignmentType]
optAligns xs ys = optLens (length xs) (length ys)
	where
		optLens i j  = optTables!!i!!j
		optTables = [[optEntrys i j | j <- [0..]] | i <-[0..]]

		optEntrys :: Int -> Int -> [AlignmentType]
		optEntrys 0 0 = [([],[])]
		optEntrys i 0 = attachHeads (xs!!((length xs)-i)) '-' (optLens (i-1) 0)
		optEntrys 0 j = attachHeads '-' (ys!!((length ys)-j)) (optLens 0 (j-1))
		optEntrys i j = maximaBy scoring ((attachHeads x y (optLens (i-1) (j-1)))++(attachHeads x '-' (optLens (i-1) j))++(attachHeads '-' y (optLens i (j-1))))
			where
				x = xs!!((length xs)-i)
				y = (ys!!((length ys)-j))