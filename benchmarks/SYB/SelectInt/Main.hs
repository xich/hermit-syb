module SYB.SelectInt.Main where

import Auxiliary.Auxiliary (test, apply)
import Data.Generics (everything)
import SYB.SelectInt.SelectInt
import TreeDatatype

mainEverythingWTree :: IO ()
{-
mainWTree = do
    mapM_ (\n -> let s = sizeWTree (mkFullWTree n)
                 in putStrLn $ show n ++ ": " ++ show s) [65,67..]
-}
mainEverythingWTree = test (putStr (show (sumWTree (apply 30 (\t -> sum (selectInt everything t) `seq` t) (mkFullWTree 53)))))

mainEverythingRWTree :: IO ()
mainEverythingRWTree = test (putStr (show (sumWTree (apply 30 (\t -> sum (selectInt everythingR t) `seq` t) (mkFullWTree 53)))))

mainEverythingQlWTree :: IO ()
mainEverythingQlWTree = test (putStr (show (sumWTree (apply 30 (\t -> sum (selectInt everythingQl t) `seq` t) (mkFullWTree 53)))))

mainEverythingQrWTree :: IO ()
mainEverythingQrWTree = test (putStr (show (sumWTree (apply 30 (\t -> sum (selectInt everythingQr t) `seq` t) (mkFullWTree 53)))))
