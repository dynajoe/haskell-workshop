module Kinds where
    
-- Kinds
data Pair a = Pair a a
-- :k Pair
-- Pair :: * -> *

-- :k Tree
-- Tree :: (* -> *) -> * -> *
data Tree k a = Node (k (Tree k a)) | Leaf a

type BinaryTree a = Tree Pair a
-- :k BinaryTree
-- BinaryTree :: * -> *

type RoseTree a = Tree [] a
