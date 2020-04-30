module Exercise2 where

import Data.List
import Data.Maybe

--Propositions
data Proposition = Var String | And Proposition Proposition | Or Proposition Proposition
	| Not Proposition

vars :: Proposition -> [String]
vars (Var s) = [s]
vars (And p1 p2) = union (vars p1) (vars p2)
vars (Or p1 p2) = union (vars p1) (vars p2)
vars (Not p) = vars p

truthValue :: Proposition -> [(String, Bool)] -> Bool
truthValue (Var s) xs = fromJust (lookup s xs)
truthValue (And p1 p2) xs = truthValue p1 xs && truthValue p2 xs
truthValue (Or p1 p2) xs = truthValue p1 xs || truthValue p2 xs
truthValue (Not p) xs = not (truthValue p xs)

--Iterate through all possible values of variables
allVals :: [String] -> [[(String, Bool)]]
allVals [] = [[]]
allVals (x:xs) = [(x, b):val | val <- allVals xs, b <- [True, False]]

tautology :: Proposition -> Bool
tautology p = and [truthValue p vals | vals <- allVals (vars p)]

--File systems
data File = File String | Directory String [File] deriving (Eq, Show)

search :: [File] -> String -> [String]
search files name = [name | File name' <- files, name==name'] ++ 
	[dir ++ "/" ++ path | Directory dir files' <- files, path <- search files' name]
	
--Sets
data Set a = Set [a] deriving (Eq, Show)

empty :: Set a
empty = Set []

add :: Eq a => a -> Set a -> Set a
add x (Set xs)
	| elem x xs = Set xs
	| otherwise = Set (x:xs)
	
combine :: Eq a => Set a -> Set a -> Set a
combine (Set xs) (Set ys) = Set (union xs ys)

remove :: Eq a => a -> Set a -> Set a
remove x (Set xs) = Set (xs \\ [x])

member :: Eq a => a -> Set a -> Bool
member x (Set xs) = elem x xs

isEmpty :: Set a -> Bool
isEmpty (Set xs) = null xs

--Ordering
-- instance (Ord a, Ord b) => Ord (a, b) where
	-- (x, y) < (z, w) = x < z || x == z && y < w
	-- (x, y) <= (z, w) = x <= z || x == z && y <= w
	-- (x, y) > (z, w) = x > z || x == z && y > w
	-- (x, y) >= (z, w) = x >= z || x == z && y >= w
	
	-- max (x, y) (z, w)
		-- | (x, y) >= (z, w) = (x, y)
		-- | otherwise = (z, w)
	
	-- min (x,y) (z, w)
		-- | (x, y) <= (z, w) = (x, y)
		-- | otherwise = (z, w)
		
	-- compare (x, y) (z, w)
		-- | (x, y) < (z, w) = LT
		-- | (x, y) > (z, w) = GT
		-- | (x, y) == (z, w) = EQ
		
-- instance Ord b => Ord [b] where
	-- [] < _ = True
	-- _ < [] = False
	
	-- (x:xs) < (y:ys) = x < y && xs < ys

	-- x <= y = x < y || x <= y
	-- x > y = x > y
	-- x >= y = x > y || x >= y
	
	-- max x y
		-- | x >= y = x
		-- | otherwise = y
		
	-- min x y
		-- | x <= y = x
		-- | otherwise = y
	
	-- compare x y
		-- | x > y = GT
		-- | x < y = LT
		-- | x == y = EQ
		
--ListNatural
newtype ListNatural = LN [()] deriving (Eq, Show)
--(:) is successor of natural numbers of nothing
-- (++) is addition of natural numbers of nothing
-- map (const ()) is toListNatural
f1 x y = foldr (:) x y -- is addition of 2 natural numbers
f2 x y = foldr (const (f1 x)) [] y -- multiplication
f3 x y = foldr (const (F2 x)) [()] y -- power of

	