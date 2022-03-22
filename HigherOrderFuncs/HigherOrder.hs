add1 :: Int -> Int
add1 x = x + 1

doubler :: Int -> Int
doubler x = x * 2

main = do
    let
        originalList = [1,2,3,4,5]
        mapResult = map (\x -> x * x) originalList
        foldlResult = foldl (\x y -> x + y) 0 originalList
        addThenDouble = doubler . add1 -- same as doubler(add1(x))
        doubleThenAdd = add1 . doubler -- same as add1(doubler(x))
    putStrLn ("Original list is " ++ (show originalList))
    putStrLn ("mapResult is " ++ (show mapResult))
    putStrLn ("foldlResult is " ++ (show foldlResult))
    putStrLn ("addThenDouble applied to originalList is " ++ (show (map addThenDouble originalList)))
    putStrLn ("doubleThenAdd applied to originalList is " ++ (show (map doubleThenAdd originalList)))