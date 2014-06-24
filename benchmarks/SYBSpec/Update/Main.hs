module SYBSpec.Update.Main where

import Auxiliary.Tree (bigTree, sumTree)
import Auxiliary.Logic (biggerLogic, evalLogic, Logic)
import Auxiliary.Auxiliary (test, apply)
import Auxiliary.SYB
import SYBSpec.Update.Update

import Data.Generics

import InlineGmap.Prelude

mainTree :: IO ()
mainTree = test (putStr (show (sumTree (apply 20 updateInt bigTree))))

mainLogic :: IO ()
mainLogic = test (putStr (show (evalLogic (\_ _ -> True) (apply 800 (updateString (const 'y')) biggerLogic))))

updateString :: (Char -> Char) -> Logic -> Logic
updateString f = everywhere (mkT f)
