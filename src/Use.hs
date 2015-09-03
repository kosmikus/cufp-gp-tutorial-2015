module Use where

import Generics.SOP

data BinTree a = Leaf a | Node (BinTree a) (BinTree a)
  deriving Show

deriveGeneric ''BinTree

