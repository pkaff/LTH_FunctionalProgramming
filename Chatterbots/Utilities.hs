module Utilities where

--map2 is a function, taking a tuple of functions f1, f2 and applying them on the tuple x1, x2 respectively
map2 :: (a -> b, c -> d) -> (a, c) -> (b, d)
map2 (f1, f2) (x1, x2) = (f1 x1, f2 x2)

--mmap makes it possible to use map on Maybes. ex. mmap succ Maybe 1 simply returns 2 if Maybe 1 was 1
--Nothing otherwise
mmap :: (a -> b) -> Maybe a -> Maybe b
mmap f  Nothing  = Nothing
mmap f (Just x)  = Just (f x)

--Compares two maybes. If both both are Nothing, return Nothing. If both are Just a, return Just first 
--argument. If Only one is Just a, return Just a.
orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing  x  = x
orElse (Just a) _  = Just a

--Returns the actual value of a function returning a Maybe. E.g. if myMaybe 10 returns Just 10, try myMaybe 
-- returns 10. If myMaybe returns Nothing (myMaybe 9), second parameter is returned (9).
try :: (a -> Maybe a) -> a -> a
try f x = maybe x id (f x)

--Recursive method which tries to fix f x to x. Works only if solution exists in the right direction
--Not sure about the usage of this (how and why)...
fix :: Eq a => (a -> a) -> a -> a
fix f x
   |  f x == x  = x
   |  otherwise = fix f (f x)

-- !! Stands for accessing a certain position in a list. The size of xs is multiplied by u and floored.
-- Pick the looks at this position in the list xs. u is supposedly 0 <= u <= 1
pick :: RealFrac r => r -> [a] -> a
pick u xs = xs !! (floor.(u*).fromIntegral.length) xs

--Sum number of hours worked on this projekt:
--Trying to understand / test utilityfunctions: 2h
--Trying to understand how to write substitute: 1h

--Completing substitute + beginning on match: 1hx2
--Completing match: 1,5hx2
--Completing Transformation(s)Apply: 0,5hx2
--Completing Reflect and RulesApply: 3,5hx2
--Completing RulesCompile and beginning on stateOfMind: 2hx2
--Completing stateOfMind: 0.5hx2