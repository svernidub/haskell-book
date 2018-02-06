module BinTree where

import Prelude (Int(..), Show(..), maximum, (+), (++))
import Nat(Nat(..))

data BinTree a = Leaf a
               | Node (BinTree a) (BinTree a)
               deriving (Show)

reverse :: BinTree a -> BinTree a
reverse (Leaf nd)                    = Leaf nd
reverse (Node (Leaf nd1) (Leaf nd2)) = Node (Leaf nd2) (Leaf nd1)
reverse (Node (Leaf nd1) nd2)        = Node (reverse nd2) (Leaf nd1)
reverse (Node nd1 (Leaf nd2))        = Node (Leaf nd2) (reverse nd1)
reverse (Node nd1 nd2)               = Node (reverse nd2) (reverse nd1)


depth :: BinTree a -> Nat
depth (Leaf nd)      = 1 :: Nat
depth (Node nd1 nd2) = (1 :: Nat) + maximum [(depth nd1), (depth nd2)]


leaves :: BinTree a -> [a]
leaves (Leaf nd)      = [nd]
leaves (Node nd1 nd2) = (leaves nd1) ++ (leaves nd2)


testTree :: BinTree Int
testTree = do
    let leaf1 = Leaf 1
        leaf2 = Leaf 2
        node1 = Node leaf1 leaf2

        leaf3 = Leaf 3
        leaf4 = Leaf 4
        node2 = Node leaf3 leaf4

        tree = Node node1 node2
    tree
