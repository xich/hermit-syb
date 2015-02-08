module Hand.SelectInt.Main where

import Auxiliary.Auxiliary (test, apply)
import Hand.SelectInt.SelectInt
import TreeDatatype

mainDumb :: IO ()
{-
mainDumb = do
    mapM_ (\n -> let s = sizeWTree (mkFullWTree n)
                 in putStrLn $ show n ++ ": " ++ show s) [65,67..]
-}
mainDumb = test (putStr (show (sumWTree (apply 30 (\t -> sum (selectIntDumb t) `seq` t) (mkFullWTree 53)))))

mainSmart :: IO ()
mainSmart = test (putStr (show (sumWTree (apply 30 (\t -> sum (selectIntSmart t) `seq` t) (mkFullWTree 53)))))
