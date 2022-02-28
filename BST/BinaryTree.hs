data BinaryTree = Null | Node (BinaryTree) Int (BinaryTree)
    deriving (Show, Eq)

insert :: BinaryTree -> Int -> BinaryTree
insert Null val = Node Null val Null
insert (Node left x right) val
  | x > val = Node nLeft x right
  | otherwise = Node left x nRight
  where
    nLeft = insert left val
    nRight = insert right val

preOrder :: BinaryTree -> [Int]
preOrder Null = []
preOrder (Node left x right) = (preOrder left) ++ [x] ++ (preOrder right)

postOrder :: BinaryTree -> [Int]
postOrder Null = []
postOrder (Node left x right) = (postOrder right) ++ [x] ++ (postOrder left)

inOrder :: BinaryTree -> [Int]
inOrder Null = []
inOrder (Node left x right) = [x] ++ (inOrder left) ++ (inOrder right)

removeMin :: BinaryTree -> (BinaryTree, Maybe Int)
removeMin Null = (Null, Nothing)
removeMin (Node Null a Null) = (Null, Just a)
removeMin (Node (Node Null b leftRightSubBT) a rightBT) = (Node leftRightSubBT a rightBT, Just b) 
removeMin (Node leftBT a _) = (minSubBT, minItem) 
    where
        (minSubBT, minItem) = removeMin leftBT

removeMax :: BinaryTree -> (BinaryTree, Maybe Int)
removeMax Null = (Null, Nothing)
removeMax (Node Null a Null) = (Null, Just a)
removeMax (Node leftBT a (Node Null b rightLeftSubBT)) = (Node leftBT a rightLeftSubBT, Just b) 
removeMax (Node _ a rightBT) = (maxSubBT, maxItem) 
    where
        (maxSubBT, maxItem) = removeMax rightBT