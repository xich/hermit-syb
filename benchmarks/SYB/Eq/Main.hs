module SYB.Eq.Main where

import Auxiliary.Auxiliary (test, apply)
import Data.Generics (geq)
import TreeDatatype

mainWTree :: IO ()
{-
mainWTree = do
    mapM_ (\n -> let s = sizeWTree (mkFullWTree n)
                 in putStrLn $ show n ++ ": " ++ show s) [65,67..]
-}
mainWTree = test (putStr (show (sumWTree (apply 60 (\t -> (t `geq` t) `seq` t) (mkFullWTree 53)))))

