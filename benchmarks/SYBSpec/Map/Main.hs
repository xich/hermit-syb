module SYBSpec.Map.Main where

import Auxiliary.Tree (smallerTree, sumTree, Tree)
import Auxiliary.Auxiliary (test, apply)
import Auxiliary.SYB

import Data.Generics

import InlineGmap.Prelude

mainTree :: IO ()
mainTree = test (putStr (show (sumTree (apply 100 (incTree (+1)) smallerTree))))

incTree :: (Int -> Int) -> (Tree Int -> Tree Int)
incTree f = everywhere (mkT f)
