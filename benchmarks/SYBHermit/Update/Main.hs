module SYBHermit.Update.Main where

import Auxiliary.Tree (bigTree, sumTree)
import Auxiliary.Logic (biggerLogic, evalLogic, Logic)
import Auxiliary.Auxiliary (test, apply, seqHsModule)
import Auxiliary.SYB
import SYBHermit.Update.Update

import Data.Generics

import Language.Haskell.Parser
import Language.Haskell.Syntax

mainTree :: IO ()
mainTree = test (putStr (show (sumTree (apply 20 updateInt bigTree))))

mainLogic :: IO ()
mainLogic = test (putStr (show (evalLogic (\_ _ -> True) (apply 800 (updateStringLogic (const 'y')) biggerLogic))))

mainHsModule :: IO ()
mainHsModule = do
    fc <- readFile "Auxiliary/Tree.hs"
    m <- case parseModule fc of
            ParseOk m -> return m
            _ -> fail "parseModule failed"
    test $ putStr $ show $ flip seq () $ apply 100 (seqHsModule . updateStringHsModule (const 'y')) m

updateStringLogic :: (Char -> Char) -> Logic -> Logic
updateStringLogic f = everywhere (mkT f)

updateStringHsModule :: (Char -> Char) -> HsModule -> HsModule
updateStringHsModule f = everywhere (mkT f)
