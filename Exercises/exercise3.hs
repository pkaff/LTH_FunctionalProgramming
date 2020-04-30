module Exercise3 where

--Simple definitions
--Sets

newtype Set a = Set [a]

sing :: a -> Set a
sing x = Set [x]

--Must define stuff like unionMult which takes the union of a list of sets
-- instance Monad Set where
	-- return = sing
	-- (Set x) >>= f = unionMult (map f x)

--Trees
data Tree a = Leaf a | Branch (Tree a) (Tree a)

instance Monad Tree where
	return = Leaf
	(Leaf x) >>= f = f x
	(Branch l1 l2) >>= f = Branch (l1 >>= f) (l2 >>= f)
	
--Error
data Error a = OK a | Error String

instance Monad Error where
	return = OK
	(Error s) >>= f = (Error s)
	(OK x) >>= f = f x
	
--Monadic helper functions
sequence' :: Monad m => [m ()] -> m ()
sequence' [] = do return ()
sequence' (m:ms) = do
	m
	sequence' ms
	
onlyIf :: Monad m => Bool -> m () -> m ()
onlyIf b m
	| b = m
	| otherwise = return ()
	
onlyIfM :: Monad m => m Bool -> m () -> m ()
onlyIfM b m =
	do 
	bool <- b
	if bool 
		then m 
		else return ()
			
--List comprehension
list1 = [(x, y) | x <- [1..], y <- [1..x]]

list2 = do
	x <- [1..]
	y <- [1..x]
	return (x, y)
	
list2' = [1..] >>= (\x -> [1..x] >>= (\y -> return (x, y)))

--Theorem proving
