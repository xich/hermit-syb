module SYBSpec.RmWeights.Main where

import Auxiliary.Auxiliary (test, apply)
import SYB.RmWeights.RmWeights
import TreeDatatype

import HERMIT.Optimization.SYB.Prelude

mainWTree :: IO ()
mainWTree = test (putStr (show (sumWTree (apply 50 (\t -> sumWTree (rmWeights t) `seq` t) (mkFullWTree 53)))))
