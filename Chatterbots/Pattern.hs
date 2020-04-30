module Pattern where
import Utilities


--------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute _ [] _ = []
substitute a (x:xs) ys
	| a == x = ys++(substitute a xs ys)
	| otherwise = x:(substitute a xs ys)

-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [][] = Just []
match _ [] _ = Nothing
match _ _ [] = Nothing
match w (x:xs) (y:ys)
	| x == w = orElse (singleWildcardMatch (x:xs) (y:ys)) (longerWildcardMatch (x:xs) (y:ys))
	| x == y = match w xs ys
	| otherwise = Nothing

-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:ps) (x:xs) = mmap ((:[]) . head) (mmap (x:) (match wc ps xs))

longerWildcardMatch (wc:ps) (x:xs) = mmap (x:) (match wc (wc:ps) xs)

-- Test cases --------------------

testPattern =  "* and *;"
testSubstitutions = "you"
testString = "you and me;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions



--------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply w f xs t = mmap (substitute w (snd t)) (mmap f (match w (fst t) xs))

-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ [] _ = Nothing
transformationsApply w f (t:ts) xs = orElse (transformationApply w f xs t) (transformationsApply w f ts xs)
