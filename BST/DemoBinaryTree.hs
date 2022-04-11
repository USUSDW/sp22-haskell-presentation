import BinaryTree

main = do
    let
        tree1 = insert Null 5
        tree2 = insert tree1 6
        tree3 = insert tree2 7
        tree4 = insert (insert (insert Null 5) 6) 7
        foldedTree = foldl insert Null [5,6,7]
        testTree = foldl insert Null [10, 2, 7, 29, 11, 17, -1, 3, 22]
        
        (treeNoMin, oldMin) = removeMin testTree
        (treeNoMax, oldMax) = removeMax testTree
        (secondTreeNoMin, secondOldMin) = removeMin treeNoMin

    putStrLn $ "Tree 1: " ++ (show tree1)
    putStrLn $ "Tree 2: " ++ (show tree2)
    putStrLn $ "Tree 3: " ++ (show tree3)
    putStrLn $ "Tree 4: " ++ (show tree4)
    putStrLn $ "Folded Tree: " ++ (show foldedTree)

    putStrLn ""
    
    putStrLn $ "Test Tree: " ++ (show testTree)
    
    putStrLn $ "Test Tree In Order: " ++ (show (inOrder testTree))
    putStrLn $ "Test Tree Pre Order: " ++ (show (preOrder testTree))
    putStrLn $ "Test Tree Post Order: " ++ (show (postOrder testTree))

    putStrLn "\n=== Remove Min Test ===\n"
    putStrLn $ "Test Tree removed min value " ++ (show oldMin)
    putStrLn $ "Test Tree without min value:\n" ++ (show treeNoMin)
    putStrLn $ "Pre order traversal of tree: " ++ (show (preOrder treeNoMin))
    
    putStrLn "\n=== Remove Max Test ===\n"
    putStrLn $ "Test Tree removed max value " ++ (show oldMax)
    putStrLn $ "Test Tree without max value:\n" ++ (show treeNoMax)
    putStrLn $ "Pre order traversal of tree: " ++ (show (preOrder treeNoMax))

    putStrLn "\n=== Remove Min (x2) Test ===\n"
    putStrLn $ "Test Tree removed second min value " ++ (show secondOldMin)
    putStrLn $ "Test Tree without first two min values:\n" ++ (show secondTreeNoMin)
    putStrLn $ "Pre order traversal of tree: " ++ (show (preOrder secondTreeNoMin))
