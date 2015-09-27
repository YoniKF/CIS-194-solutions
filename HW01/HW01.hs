{-# OPTIONS_GHC -Wall #-}

-- Exercise 1
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 	= []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]
	
toDigitsRev :: Integer -> [Integer]
toDigitsRev n = rev (toDigits n)
  where rev []     = []
        rev (x:xs) = reverse xs ++ [x]

-- Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []  = []
doubleEveryOther (n:ns)
  | odd (length ns) = 2*n : doubleEveryOther ns
  | otherwise       = n :doubleEveryOther ns
    
-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (n:ns)
  | n < 10    = n + sumDigits ns
  | otherwise = sumDigits (toDigits n ++ ns)
    
-- Exercise 4
validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0

-- Exercise 5
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n == 0    = []
  | n == 1    = [(a,b)]
  | otherwise = hanoi (pred n) a c b ++ [(a,b)] ++ hanoi (pred n) c b a

{-    
-- Exercise 6 (Optional)
hanoi' :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi' n a b c d
    | n == 0    = []
    | n == 1    = [(a,b)]
    | otherwise = hanoi' (rest `div` 2) a c b d ++ hanoi' (rest - rest `div` 2) a d b c ++ [(a,b)]
                    ++ hanoi' (rest - rest `div` 2) d b a c ++ hanoi' (rest `div` 2) c b a d
    where rest = pred n
-}    
