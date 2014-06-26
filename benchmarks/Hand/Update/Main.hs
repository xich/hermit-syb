module Hand.Update.Main where

import Auxiliary.Tree (bigTree, sumTree)
import Auxiliary.Logic (biggerLogic, evalLogic)
import Auxiliary.Auxiliary (test, apply, seqHsModule)
import Hand.Update.Update

import Language.Haskell.Parser
import Language.Haskell.Syntax

mainTree :: IO ()
mainTree = test . putStr . show . sumTree . (apply 20 updateTree) $ bigTree

mainLogic :: IO ()
mainLogic = test . putStr . show . evalLogic (\_ _ -> True) . (apply 800 (updateLogic (const 'y'))) $ biggerLogic

mainHsModule :: IO ()
mainHsModule = do
    fc <- readFile "Auxiliary/Tree.hs"
    m <- case parseModule fc of
            ParseOk m -> return m
            _ -> fail "parseModule failed"
    test $ putStr $ show $ flip seq () $ apply 100 (seqHsModule . updateHsModule) m
