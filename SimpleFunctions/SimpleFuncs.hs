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
