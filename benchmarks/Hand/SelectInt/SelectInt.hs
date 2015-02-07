module Hand.SelectInt.SelectInt where

import TreeDatatype

selectInt :: WTree Int Int -> [Int]
selectInt (Leaf x) = [x]
selectInt (Fork l r) = selectInt l ++ selectInt r
selectInt (WithWeight t w) = w : selectInt t

selectInt_acc :: WTree Int Int -> [Int]
selectInt_acc = loop []
  where
    loop acc (Leaf x) = x : acc
    loop acc (Fork l r) = loop (loop acc r) l
    loop acc (WithWeight t w) = loop (w : acc) t
