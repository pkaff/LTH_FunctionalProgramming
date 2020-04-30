module StringAlignment where

import Data.List

--Recursive similarityScore
similarityScore :: String -> String -> Int
similarityScore [] [] = 0
similarityScore [] (y:ys) = (length (y:ys)) * scoreSpace
similarityScore (x:xs) [] = (length (x:xs)) * scoreSpace
similarityScore (x:xs) (y:ys) = maximum [score x y + similarityScore xs ys, 
	scoreSpace + similarityScore xs (y:ys), scoreSpace + similarityScore (x:xs) ys]

--Optimization using matrix
similarityScore' :: String -> String -> Int
similarityScore' xs ys = simSc (length xs) (length ys)
	where
		simSc i j = simTable!!i!!j
		simTable = [[simEntry i j | j <- [0..]] | i <- [0..]]
		
		simEntry :: Int -> Int -> Int
		simEntry i 0 = i * scoreSpace
		simEntry 0 j = j * scoreSpace
		simEntry i j = maximum [score x y + simSc (i - 1) (j - 1), score x y + simSc i (j - 1),
			score x y + simSc (i - 1) j]
				where
					x = xs!!(i - 1)
					y = ys!!(j - 1)

--Scoring function
score :: Char -> Char -> Int
score '-' y = scoreSpace
score x '-' = scoreSpace
score x y
	| x == y = scoreMatch
	| otherwise = scoreMismatch

--Scores
scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1

--Given a list of tuples of lists, we can prepend two elements of the same type to all of the tuples
--in this list using this function. 
attachHeads :: a -> a -> [([a], [a])] -> [([a], [a])]
attachHeads h1 h2 aList = [(h1:xs, h2:ys) | (xs, ys) <- aList]

--Tails for optimization
attachTails :: a -> a -> [([a], [a])] -> [([a], [a])]
attachTails t1 t2 aList = [(xs++[t1], ys++[t2]) | (xs, ys) <- aList]

--Returns all the maximum values according to valueFcn
maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = filter ((==maximum [valueFcn x | x <- xs]) . valueFcn) xs

type AlignmentType = (String, String)

optAlignments :: String -> String -> [AlignmentType]
optAlignments [] [] = [([], [])]
optAlignments (x:xs) [] = attachHeads x '-' (optAlignments xs [])
optAlignments [] (y:ys) = attachHeads '-' y (optAlignments [] ys)
optAlignments (x:xs) (y:ys) = maximaBy stringScore alignments
	where 
		part1 = attachHeads x y (optAlignments xs ys)
		part2 = attachHeads x '-' (optAlignments xs (y:ys))
		part3 = attachHeads '-' y (optAlignments (x:xs) ys)
		alignments = part1 ++ part2 ++ part3
		stringScore (str1, str2) = sum (zipWith score str1 str2)
		
--Optimization using matrix, suboptimal scoring
optAlignments'' :: String -> String -> [AlignmentType]
optAlignments'' xs ys = optAlig (length xs) (length ys)
	where
		optAlig i j = aligTable!!i!!j
		aligTable = [[aligEntry i j | j <- [0..]] | i <- [0..]]
		
		aligEntry :: Int -> Int -> [AlignmentType]
		aligEntry i 0 = [(take i xs, ['-' | a <- [1..i]])]
		aligEntry 0 j = [(['-' | a <- [1..j]], take j ys)]
		aligEntry i j
			| x == y = part1
			| otherwise = maximaBy stringScore alignments
				where
					x = xs!!(i - 1)
					y = ys!!(j - 1)
					part1 = attachTails x y (optAlig (i - 1) (j - 1))
					part2 = attachTails x '-' (optAlig (i - 1) j)
					part3 = attachTails '-' y (optAlig i (j - 1))
					alignments = part1 ++ part2 ++ part3
					stringScore (str1, str2) = sum (zipWith score str1 str2)
					
--Optimization using matrix with scoring
optAlignments' :: String -> String -> [AlignmentType]
optAlignments' xs ys = snd (optAlig (length xs) (length ys))
	where
		optAlig i j = aligTable!!i!!j
		aligTable = [[aligEntry i j | j <- [0..]] | i <- [0..]]
		
		aligEntry :: Int -> Int -> (Int, [AlignmentType])
		aligEntry i 0 = (i * scoreSpace, [(take i xs, replicate i '-')]) --x and y are not visible here, using scoreSpace seems cleaner to me
		aligEntry 0 j = (j * scoreSpace, [(replicate j '-', take j ys)])
		aligEntry i j = (maxScore, combinedAlignments)
				where
					x = xs!!(i - 1)
					y = ys!!(j - 1)

					maxedAlignments = maximaBy fst [
						newEntry x y (optAlig (i - 1) (j - 1)), 
						newEntry x '-' (optAlig (i - 1) j), 
						newEntry '-' y (optAlig i (j - 1))
						]
					
					combinedAlignments = concat (map snd maxedAlignments)

					maxScore = fst (head (maxedAlignments))
					
					newEntry :: Char -> Char -> (Int, [AlignmentType]) -> (Int, [AlignmentType])
					newEntry x y (s, alts) = (s + score x y, attachTails x y alts)

					
outputOptAlignments :: String -> String -> IO ()
outputOptAlignments [] [] = return ()
outputOptAlignments str1 str2 = do
	putStrLn "These are the optimal Alignments:\n"
	let alignments = optAlignments' str1 str2
	putStrLn . unlines .map showAlign $ alignments

showAlign :: AlignmentType -> String
showAlign (str1, str2) = str1 ++ "\n" ++ str2 ++ "\n"

				
--Time worked:
--1h
--2h
--2h
--4h
--5h