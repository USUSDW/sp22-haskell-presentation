--  Defines a function that takes no arguments and returns 5
someFunc = 5

--  Defines a function that takes no arguments and returns 5
--    *using explicit typing*
someFuncTyped :: Int
someFuncTyped = 5

--  Defines a function that takes number n and returns n + 2
add2 n = n + 2

--  Create add2 function, but with explicit typing
add2Typed :: Int -> Int
add2Typed n = n + 2

--  Make a tuple of any elements
--  Given object of type a, return a tuple of two objects of type a
makeTuple :: a -> (a, a)
makeTuple givenItem = (givenItem, givenItem)

--  Make a fibonacci function that uses pattern matching
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = n * (fib (n - 1))

--  Make a fibonacci function using guards
guardedFib :: Int -> Int
guardedFib n
    | n <= 1    = 1
    | otherwise = n * (guardedFib (n - 1))

