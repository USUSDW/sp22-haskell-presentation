main = do
    let
        originalList = [1,2,3,4,5]
        mapResult = map (\x -> x * x) originalList
        foldlResult = foldl (\x y -> x + y) 0 originalList
    putStrLn ("Original list is " ++ (show originalList))
    putStrLn ("mapResult is " ++ (show mapResult))
    putStrLn ("foldlResult is " ++ (show foldlResult))