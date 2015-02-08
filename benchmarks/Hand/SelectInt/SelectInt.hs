module Hand.SelectInt.SelectInt where

import TreeDatatype

selectIntSmart :: WTree Int Int -> [Int]
selectIntSmart (Leaf x) = [x]
selectIntSmart (Fork l r) = selectIntSmart l ++ selectIntSmart r
selectIntSmart (WithWeight t w) = w : selectIntSmart t

selectIntDumb :: WTree Int Int -> [Int]
selectIntDumb (Leaf x) = [x]
selectIntDumb (Fork l r) = selectIntDumb l ++ selectIntDumb r
selectIntDumb (WithWeight t w) = selectIntDumb t ++ [w]

selectInt_acc :: WTree Int Int -> [Int]
selectInt_acc = loop []
  where
    loop acc (Leaf x) = x : acc
    loop acc (Fork l r) = loop (loop acc r) l
    loop acc (WithWeight t w) = loop (w : acc) t
