module LinkedList where

data LinkedList a = EmptyList | Node a (LinkedList a)
    deriving (Show, Eq)
    -- deriving (Show, Eq) allows us to *show* the structure of our data and Eq allows us to check
    -- if two LinkedLists are *structurally equivelant*

-- Insert a new element into a linked list
-- To construct a new list, invoke 'insert EmptyList itemToAdd'
insert :: (LinkedList a) -> a -> (LinkedList a)
insert EmptyList item = Node item EmptyList
insert ( Node b EmptyList ) item = Node b ( Node item EmptyList )
insert ( Node b ( Node c tailList ) ) item = Node b ( Node c ( insert tailList item ) )

removeLast :: (LinkedList a) -> (LinkedList a, Maybe a)
removeLast EmptyList = (EmptyList, Nothing)
removeLast (Node lastItem EmptyList) = (EmptyList, Just lastItem)
removeLast (Node a tailList) = (Node a newTailList, lastItem)
    where
        (newTailList, lastItem) = removeLast tailList 

--  Remove the first element from the list and return a tuple with new linked list and the item that was removed
--    Returns (NewLinkedList, ItemToReturn)
--    NewLinkedList will be EmptyList if given list was empty or of size 1
--    ItemToReturn will be Maybe a
--      'Nothing' if given list is EmptyList, 
--      'Just a' otherwise 
removeFirst :: (LinkedList a) -> (LinkedList a, Maybe a)
removeFirst EmptyList = (EmptyList, Nothing)
removeFirst (Node item remainder) = (remainder, Just item)

--  Remove a specified element from our list and return a tuple with resulting LinkedList and a Bool indicating if something was removed
removeElement :: Eq a => (LinkedList a) -> a -> (LinkedList a, Bool)
removeElement EmptyList item = (EmptyList, False)
removeElement (Node a tail) item
    | a == item = (tail, True)
    | otherwise = (Node a rmResList, rmResBool) 
        where
            (rmResList, rmResBool) = removeElement tail item

-- Get our linked list as a naive list
asList :: (LinkedList a) -> [a]
asList EmptyList = []
asList (Node item remainder) = [item] ++ asList remainder

