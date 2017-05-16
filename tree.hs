module BinaryTree where

data BinaryTree a = Leaf 
                | Node (BinaryTree a) a (BinaryTree a)
                deriving (Eq,Ord,Show)


insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf 
insert' b (Node left a right)
    | b == a = Node left a right 
    | b < a = Node (insert' b left) a right 
    | b > a = Node left a (insert' b right)

-- Map for Binary Tree
mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right)= 
        Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

-- Acceptance test for map tree
mapOkay =
    if mapTree (+1) testTree' == mapExpected
    then print "yup okay!"
    else error "test failed!"

-- turn Binary Tree into list
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left val right) = [val] 
                                ++ preorder left 
                                ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left val right) = inorder left ++ 
                                [val] ++ 
                                inorder right

postorder :: Ord a => BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left val right) = postorder left 
                                ++ postorder right
                                ++ [val]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO()
testPreorder = 
    if preorder testTree == [2,1,3]
    then putStrLn "Preorder fine!"
    else putStrLn "Bad preorder"

testInOrder :: IO()
testInOrder = 
    if inorder testTree == [1,2,3]
    then putStrLn "Inorder fine!"
    else putStrLn "Inorder Bad"

testPostOrder :: IO()
testPostOrder = 
    if postorder testTree == [1,3,2]
    then putStrLn "Postorder fine!"
    else putStrLn "postorder failed check"

main :: IO()
main = do 
    testPreorder
    testInOrder
    testPostOrder


-- preorder traversal
foldTree :: (a -> b -> b -> b) -> b -> BinaryTree a -> b 
foldTree _ s Leaf = s 
foldTree f s (Node left val right) = f val (foldTree f s left) (foldTree f s right) 

-- using foldTree
mapTree' :: (a -> b) -> BinaryTree a -> BinaryTree b 
mapTree' f bt = foldTree 
            (\x y z -> (Node y (f x) z))
            Leaf
            bt 


























