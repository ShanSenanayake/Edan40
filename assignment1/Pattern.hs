module Pattern where
import Utilities
import Data.Maybe


-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute n [] ys = []
substitute n (x:xs) ys 
	| n == x = ys++substitute n xs ys
	| otherwise = x:substitute n xs ys
{- TO BE WRITTEN -}


-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ [] _ = Nothing
match _ _ [] = Nothing
match n (x:xs) (y:ys)
	| x == n = orElse(singleWildcardMatch (x:xs) (y:ys)) (longerWildcardMatch (x:xs) (y:ys))
	| x == y = match n xs ys
	| otherwise = Nothing
{- TO BE WRITTEN -}


-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:ps) (x:xs) 
	| isJust (match wc ps xs) = Just [x]
	| otherwise = Nothing 
{- TO BE WRITTEN -}
longerWildcardMatch _ [] = Nothing
longerWildcardMatch (wc:ps) (x:xs) 
	| isJust(hej) = Just (x:fromJust(hej))
	| otherwise = Nothing
	where hej = (match wc (wc:ps) xs) 
{- TO BE WRITTEN -}



-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions



-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply wc f xs (t1,t2) = mmap (substitute wc t2) (mmap f (match wc t1 xs))

{- TO BE WRITTEN -}


-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ [] _ = Nothing
transformationsApply wc f (x:xs) ys = orElse(transformationApply wc f ys x) (transformationsApply wc f xs ys)
{- TO BE WRITTEN -}

