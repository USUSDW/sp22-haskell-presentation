import LinkedList

main :: IO ()
main = do
    let 
        list1 = foldl insert EmptyList [2, 4, 6, 8]
        (newList1, lastItem) = removeLast list1
        (newList2, firstItem) = removeFirst list1
    putStrLn ("Have original list " ++ (show list1))
    putStrLn ("Original list1 as list: " ++ (show (asList list1)))
    putStrLn ("Last item was " ++ (show lastItem))
    putStrLn ("Have newList1 which is list1 with last item removed: " ++ (show newList1))
    putStrLn ("First item was " ++ (show firstItem))
    putStrLn ("Have newList2 which is list1 with first item removed: " ++ (show newList2))
    