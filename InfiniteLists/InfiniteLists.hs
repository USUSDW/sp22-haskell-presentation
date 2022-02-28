module InfiniteLists where

naturalNumbers :: [Int]
naturalNumbers = [1..]

wholeNumbers :: [Int]
wholeNumbers = [0] ++ naturalNumbers

evenNumbers :: [Int]
evenNumbers = [2 * x | x <- naturalNumbers]

squaredNumbers :: [Int]
squaredNumbers = [x * x | x <- naturalNumbers]

primes :: [Int]
primes = 2 : 3 : [x | x <- [2..], all (\p -> (x `mod` p) /= 0) (takeWhile (\p -> p * p <= x) primes)]
-- Prime numbers are numbers 2, 3, and x such that x comes from the list [2..] and x is not divisible by any of the primes p such that p * p <= x   
-- Horribly ineffecient, but it works.
