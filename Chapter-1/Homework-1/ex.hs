-- Exercise1
toDigits :: Integer -> [Integer]
toDigits n
 | n <= 0 = []
 | otherwise = toDigits (div n 10) ++ [mod n 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n 
 | n <= 0 = []
 | otherwise = mod n 10 : toDigitsRev (div n 10)

-- Exercise2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:xs) = x*2:y:doubleEveryOther xs

-- Exercise3
sumHelper :: Integer -> Integer
sumHelper n
 | n == 0 = 0
 | n > 0 = mod n 10 + sumHelper (div n 10)
 | otherwise = 0 

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sumHelper x + sumDigits xs

-- Exercie 4
valiHelper :: Integer -> Bool
valiHelper n 
 | mod n 10 == 0 = True
 | otherwise = False
validate :: Integer -> Bool
validate n = valiHelper (sumDigits (doubleEveryOther (toDigits n))) 


-- Exercise 5
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a


{-
Exercise 6 (Optional) What if there are four pegs instead of three?
That is, the goal is still to move a stack of discs from the first peg to
the last peg, without ever placing a larger disc on top of a smaller
one, but now there are two extra pegs that can be used as “temporary” storage instead of only one. Write a function similar to hanoi
which solves this problem in as few moves as possible.
It should be possible to do it in far fewer moves than with three
pegs. For example, with three pegs it takes 215 − 1 = 32767 moves
to transfer 15 discs. With four pegs it can be done in 129 moves. (See
Exercise 1.17 in Graham, Knuth, and Patashnik, Concrete Mathematics,
second ed., Addison-Wesley, 1994.)
-}

-- TODO:
