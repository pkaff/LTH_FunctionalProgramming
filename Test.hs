module Main where
main = putStrLn "Hello, world!"

--

doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100
	then x
	else x*2
