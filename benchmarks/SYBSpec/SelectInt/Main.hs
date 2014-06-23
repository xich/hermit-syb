{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module SYBSpec.SelectInt.Main where

import Auxiliary.Auxiliary (test, apply)
import SYBSpec.SelectInt.SelectInt
import TreeDatatype

import InlineGmap.Prelude

mainWTree :: IO ()
mainWTree = test (putStr (show (sumWTree (apply 30 (\t -> sum (selectInt_acc t) `seq` t) (mkFullWTree 53)))))
