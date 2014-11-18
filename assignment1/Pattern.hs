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



-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ [] _ = Nothing
match _ _ [] = Nothing
match n list1@(x:xs) list2@(y:ys)
	| x == n = orElse(singleWildcardMatch list1 list2) (longerWildcardMatch list1 list2)
	| x == y = match n xs ys
	| otherwise = Nothing



-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:ps) (x:xs) 
	| isJust (match wc ps xs) = Just [x]
	| otherwise = Nothing 

longerWildcardMatch _ [] = Nothing
longerWildcardMatch list@(wc:ps) (x:xs) 
	| isJust(m) = Just (x:fromJust(m))
	| otherwise = Nothing
	where m = (match wc list xs) 




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
transformationApply wc f xs (t1,t2) = mmap (substitute wc t2) . mmap f . match wc t1 $ xs




-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ [] _ = Nothing
transformationsApply wc f (x:xs) ys = orElse(transformationApply wc f ys x) (transformationsApply wc f xs ys)


