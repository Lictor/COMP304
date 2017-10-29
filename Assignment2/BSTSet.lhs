\begin{code}
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

-- this pass in 1 element and generate a tree with a root and two empty children
singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree  

-- this funciton makes a tree from a list, it utilise the add function to sort the tree leaves
makeTree :: (Ord a) => [a] -> Tree a
makeTree [x] = singleton x
makeTree (x:xs) = add x (makeTree xs)

-- traverse down the BST and check if the given element is part of the tree
has :: (Ord a) => a -> Tree a -> Bool  
has x EmptyTree = False  
has x (Node a left right)  
    | x == a = True  
    | x < a  = has x left  
    | x > a  = has x right

-- return the number of non empty nodes in the tree
card :: Tree a -> Int
card EmptyTree = 0
card (Node a left right) = card left + card right + 1

-- add an element in the right place of the tree by comparing with nodes
add :: (Ord a) => a -> Tree a -> Tree a  
add x EmptyTree = singleton x  
add x (Node a left right)   
    | x == a = Node x left right  
    | x < a  = Node a (add x left) right  
    | x > a  = Node a left (add x right)

--the delete function does not work
del :: (Ord a) => a -> Tree a -> Tree a
del x EmptyTree = EmptyTree
del x (Node a left right)
    | x < a  = Node a (del x left) right  
    | x > a  = Node a left (del x right)
--need to consider the case when removing a node with no child, 1 child and 2 children

-- helper functions for getting the left and right child
getLeftChild :: Tree a -> Tree a
getLeftChild EmptyTree = EmptyTree
getLeftChild (Node a left right) = left

getRightChild :: Tree a -> Tree a
getRightChild EmptyTree = EmptyTree
getRightChild (Node a left right) = right
\end{code}

Discussion:
This is the binary search tree set, I have managed to create the tree, but I don't think the tree is fully balanced. But that depends on which tree node is added first. I have only implemented the make tree, add, has and card function. 