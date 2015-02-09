module Hand.Eq.Main where

import Auxiliary.Auxiliary (test, apply)
import TreeDatatype

mainWTree :: IO ()
mainWTree = test (putStr (show (sumWTree (apply 60 (\t -> (t == t) `seq` t) (mkFullWTree 53)))))
