-- possible solutions (or sketches of solutions) to exercises from class 3, EDAN40
-- 28.11.2013, Jacek Malec

----------------------------------------------------------------
-- 2.1. simple definitions
-- of course, a solution is dependent on how the type is defined

------------------------------- SETS
newtype Set a = Set [a]

empty :: Set a
empty = Set []

sing :: a -> Set a
sing x = Set [x]

memSet :: (Eq a) => a -> Set a -> Bool
memSet _ (Set []) = False
memSet x (Set xs)
    | elem x xs = True
    | otherwise = False

{-
makeSet :: (Eq a) => [a] -> Set a
makeSet [] = empty
makeset (x:xs) = union (sing x) (makeSet xs)
-- etc
-- we need the obvious stuff:

union        :: Set a -> Set a -> Set a
unionMult    :: [ Set a ] -> Set a
intersection :: Set a -> Set a -> Set a
subSet       :: Set a -> Set a -> Bool
mapSet       :: (a -> b) -> Set a -> Set b
mapset f (Set xs) = makeSet (map f xs)
-}

-- now making it a monad:
instance Monad Set where
   return = sing
   (Set x) >>= f =  unionMult (map f x)

-- remember to verify the monad laws at this point!

-- 1. return x >>= f = (Set [x]) >>= f = unionMult (map f [x]) = unionMult [ (f x) ] = f x
-- 2. (Set [xs]) >>= return = unionMult (map return [xs]) = unionMult [ys] = Set [xs]
--       where [ys] is a list of Set [x] where x are elements of Set [xs]
-- and 3. associativity, left for the reader

------------------------------ TREES

data Tree a = Leaf a | Node (Tree a) (Tree a)
-- so our trees are binary and may store values only in leaves; 
-- yours may be different,
-- however, allowing values elsewhere complicates bind
instance Monad Tree where
   return = Leaf
   (Leaf t) >>= f   = f t
   (Node l r) >>= f = Node (l >>= f) (r >>= f)

-- good discussion and links on this topic on stackoverflow

------------------------------ ERROR

data Error a = OK a | Error String
-- or 
-- newtype Error a = Either a String

instance Monad Error where
  return = OK
  (OK x) >>= f  = f x
  (Error x) >>= f = (Error x)

-- how about monad laws?

----------------------------------------------------------------
-- 2.2. Monadic helper functions

sequence_ :: Monad m => [m ()] -> m ()

-- first take:
sequence_ xs = do sequence xs
                  return ()
-- second take:
sequence2_ xs = foldr (>>) (return())
-- third take:
sequence3_ []     = do return ()
sequence3_ (m:ms) = do m
                       sequence3_ ms


----------------

onlyIf :: Monad m => Bool -> m () -> m ()

onlyIf True  m = m
onlyIf False _ = return ()

----------------

onlyIfM :: Monad m => m Bool -> m () -> m ()

onlyIfM t m = do result <- t
                 if result 
                    then m
                    else return ()

----------------------------------------------------------------
-- 2.3. List comprehension

list1 = [ (x,y) | x <- [1..], y <- [1..x]]

list2 = do
  x <- [1..]
  y <- [1..x]
  return (x,y)

list2' = [1..] >>= (\x -> [1..x] >>= (\y -> return (x,y)))


----------------------------------------------------------------
-- 2.4. Some theorem proving

-- return >@> f = f
-- f >@>> return = f
-- (f >@> g) >@> h = f >@> (g >@> h)
--
-- or
--
-- (return x) >>= f = f x
-- m >>= return = m
-- (m >>= f) >>= g = m >>= \x -> (f x >>= g)

----------------- Task 1
-- Id monad

instance Monad Id where 
    return x     = Id x 
    (Id x) >>= f = f x

{-
=== denotes equivalence

(return x) >>= f === (Id x) >>= f === f x
m >>= return === (Id x) >>= return === return x === (Id x) === m
LHS3: (m >>= f) >>= g === ((Id x) >>= f ) >>= g === (f x) >>= g 
RHS3: m >>= \x -> (f x >>= g) === (Id x) >>= \x -> (f x >>= g) === (f x >>= g)
-}

-- [] monad

instance Monad [] where
    return x = [x]
    xs >>= f = concat (map f xs)

{-
(return x) >>= f === [x] >>= f === concat (map f [x]) === concat [f x] === f x
m >>= return === xs >>= return === concat (map return xs) === concat (map return [x1, x2, ...]) ===
   === concat [return x1, return x2, ...] === concat [[x1], [x2], ...] === [x1, x2, ...] === xs === m
LHS3: (m >>= f) >>= g === (xs >>= f) >>= g === concat (map f xs) >>= g === 
      === concat (map g (concat (map f xs)))
RHS3: m >>= \x -> (f x >>= g) === xs >>= \x -> (f x >>= g) === concat (map \x -> (f x >>= g) xs) ===
      === concat (map \x -> (concat (map g (f x))) xs) ===
3 NOT FINISHED !!!
-}

-- Maybe monad

instance Monad Maybe where 
    return x      = Just x 
    Just x >>= f  = f x 
    Nothing >>= f = Nothing

{-
(return x) >>= f === Just x >>= f === f x
m >>= return === 
1. m = Just x:  (Just x) >>= return === return x === (Just x) === m
2. m = Nothing; Nothing >>= return === Nothing === m
LHS3: (m >>= f) >>= g ===
RHS3: m >>= \x -> (f x >>= g) ===
3 NOT FINISHED !!!
-}

----------------- Task 2
{-
fmap (f . g) m 
  = do x <- m
       return (f (g x))

(fmap f . fmap g) m 
  = fmap f (fmap g m)  -- def of fmap f
  = do y <- fmap g m
       return (f y)
  = do x <- m          -- def of fmap g
       y <- return (g x)
       return (f y)
  = do x <- m
       return (f (g x))
-}
----------------- Task 3
{-
join . fmap return z
  = join . {do x <- z; return (return x)}
  = do y <- {do x <- z; return (return x)}
       y
  = do y <- return z
       y
  = z

join return z
  = do y <- (return z)
       y
  = z

join (return x) 
  = do y <- (return x) 
       y
  = x
  = id x
-}
----------------- Task 4
mapLists f m =  [ f x | x <- m ]
joinLists m  = [ y | x <- m, y <- x ]

----------------------------------------------------------------
-- 2.5. State monad

{-- snippet RandomState --}
type RandomState a = State StdGen a
{-- /snippet RandomState --}

{-- snippet getRandom --}
getRandom :: Random a => RandomState a
getRandom =
  get >>= \gen ->
  let (val, gen') = random gen in
  put gen' >>
  return val
{-- /snippet getRandom --}

{-- snippet getRandomDo --}
getRandomDo :: Random a => RandomState a
getRandomDo = do
  gen <- get
  let (val, gen') = random gen
  put gen'
  return val
{-- /snippet getRandomDo --}
