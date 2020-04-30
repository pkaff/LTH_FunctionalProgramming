-- class2.hs
-- updated 12.11.2013, 12:50 JM
-- some solutions to class2 exercises, EDAN40
import Data.Maybe
import Data.List

-- safe division
------------------------------------------------------------------------------
-- Safe division that returns 'Nothing' in case of a division by zero 
-- 
safeDiv	:: Int -> Int -> Maybe Int 

safeDiv x 0 = Nothing
safeDiv x y = Just (x `div` y)

------------------------------------------------------------------------------

-- propositional logic
-- taken from: http://www.cse.chalmers.se/edu/course/TDA555/Answers/AnswersWeek5.hs
-- 3. Exercises on Propositional Logic

data Proposition = Var Name
                 | Proposition :&: Proposition
                 | Proposition :|: Proposition
                 | Not Proposition
 deriving ( Eq, Show )

type Name = String

---

vars :: Proposition -> [Name]
vars (Var x)   = [x]
vars (a :&: b) = vars a `union` vars b
vars (a :|: b) = vars a `union` vars b
vars (Not a)   = vars a

truthValue :: [(Name,Bool)] -> Proposition -> Bool
truthValue val (Var x)   = fromJust (lookup x val)
truthValue val (a :&: b) = truthValue val a && truthValue val b
truthValue val (a :|: b) = truthValue val a || truthValue val b
truthValue val (Not a)   = not (truthValue val a)

---

-- allVals xs enumerates all possible valuations of the variables xs:
--    1. when xs = [], there is just one valuation
--    2. otherwise, we enumerate all possible valuations for the rest
--       of the variables, plus all possible values of x
allVals :: [Name] -> [[(Name,Bool)]]
allVals []     = [[]]
allVals (x:xs) = [ (x,b):val
                 | val <- allVals xs
                 , b <- [False,True]
                 ]

tautology :: Proposition -> Bool
tautology a =
  and [ truthValue val a | val <- allVals (vars a) ]

-- an example
hamlet :: Proposition
hamlet = Var "to be" :|: Not (Var "to be")


------------------------------------------------------------------------------

-- file systems
-- taken from: http://www.cse.chalmers.se/edu/course/TDA555/Answers/AnswersWeek6.hs
-- 2. File Systems

data File
  = File String
  | Dir String [File]
 deriving ( Eq, Show )

type FileSystem = [File]

-- this function returns all paths
search :: FileSystem -> String -> [String]
search files name =
  [ name
  | File name' <- files
  , name == name'
  ] ++
  [ dir ++ "/" ++ path
  | Dir dir files' <- files
  , path <- search files' name
  ]

-- this function returns maybe a path
searchMaybe :: FileSystem -> String -> Maybe String
searchMaybe files name =
  listToMaybe
  ( [ name
    | File name' <- files
    , name == name'
    ] ++
    [ dir ++ "/" ++ path
    | Dir dir files' <- files
    , Just path <- [searchMaybe files' name]
    ]
  )

-- it can also be defined using the first one...
searchMaybe' :: FileSystem -> String -> Maybe String
searchMaybe' files name = listToMaybe (search files name)

exampleFileSystem :: FileSystem
exampleFileSystem =
  [ File "apa"
  , Dir "bepa" [ File "apa", Dir "bepa" [], Dir "cepa" [ File "bepa" ] ]
  , Dir "cepa" [ Dir "bepa" [], Dir "cepa" [ File "apa" ] ]
  ]



------------------------------------------------------------------------------

-- sets
-- taken from: http://www.cse.chalmers.se/edu/course/TDA555/Answers/AnswersWeek6.hs
-- 3. Simple Sets

data Set a = Set [a]
 deriving ( Show )
 
empty :: Set a
empty = Set []

add :: Eq a => a -> Set a -> Set a
add x (Set xs) | x `elem` xs = Set xs
               | otherwise   = Set (x:xs)

remove :: Eq a => a -> Set a -> Set a
remove x (Set xs) = Set (xs \\ [x])

combine :: Eq a => Set a -> Set a -> Set a
Set xs `combine` Set ys = Set (xs ++ ys)   -- bug!
--Set xs `combine` Set ys = Set (xs `union` ys) -- correct

member :: Eq a => a -> Set a -> Bool
member x (Set xs) = x `elem` xs

isEmpty :: Set a -> Bool
isEmpty (Set xs) = null xs

------------------------------------------------------------------------------

-- ordering
-- note that you have to comment this section out to get the file type-checked,
-- as those two conflict with Data.Tuple and GHC.Base, respectively

-- instance (Ord a, Ord b) => Ord (a,b) where 
--   (x,y) < (z,w)       =  x<z || x==z && y<w
--   (x,y) <= (z,w)      =  x<=z || x==z && y<=w
--   (x,y) > (z,w)       =  x>z || x==z && y>w 
--   (x,y) >= (z,w)      =  x>=z || x==z && y>=w 
--   max (x,y) (z,w)
--                     | (x,y) >= (z,w) = (x,y)
--                     | otherwise      = (z,w)
--   min (x,y) (z,w) 
--                     | (x,y) <= (z,w) = (x,y)
--                     | otherwise      = (z,w)
--   (x,y) `compare` (z,w)
--                     | (x,y) == (z,w) = EQ
--                     | (x,y) < (z,w)  = LT
--                     | (x,y) > (z,w)  = GT


-- instance Ord b => Ord [b] where 
--   [] < _                  = True
--   _  < []                 = False
--   (x:xs) < (y:ys)       = x < y && xs < ys
--   x <= y      = x < y || x == y
--   x > y      = y < x
--   x >= y     = y <= x
--   x `max` y    
--              | x >= y = x
--              | otherwise = y
--   x `min` y    
--              | x >= y = y
--              | otherwise = x
--   x `compare` y
--              | x == y = EQ
--              | x > y = GT
--              | x < y = LT

------------------------------------------------------------------------------

-- ListNatural

-- (:) is Succ
-- (++) is add
-- map (const()) is toListNatural (coerce)
-- f1 is add
-- f2 is multiply
-- f3 is power

newtype ListNatural = LN [()] deriving (Show, Eq)      

-- conversion has to be defined here
-- alternatively take the path of MyNatural and define everything by hand

zeroJM = LN []
zeropJM (LN x) = (x == [])
plusJM (LN x) (LN y) = LN (x++y)
incJM (LN x) = LN (():x)
decJM (LN x) 
   | zeropJM (LN x) = error "no negative naturals exist"
   | otherwise = LN (tail x)
minusJM (LN x) (LN y) 
   | (length x) < (length y) = error "no negative naturals exist"
   | zeropJM (LN y) = LN x
   | otherwise = minusJM (decJM (LN x)) (decJM (LN y)) 
timesJM (LN x) (LN y) = LN (concatMap (const x) y)
absJM (LN x) = (LN x)
signumJM (LN x) 
   | zeropJM (LN x) = zeroJM
   | otherwise = incJM zeroJM
toIntegerJM (LN x) = (length x)
fromIntegerJM x 
   | x < 0  = error "no negative naturals exist"
   | x == 0 = zeroJM
   | otherwise = LN (toList x) where
        toList 0 = []
        toList n = ():(toList (n-1))

instance Num ListNatural where
   (+) = plusJM
   (*) = timesJM
   (-) = minusJM
   abs = absJM
   signum = signumJM
   negate = error "no negative naturals exist"
   fromInteger = fromIntegerJM
