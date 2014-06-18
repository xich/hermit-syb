module SYBSpec.Update.Update where

import Auxiliary.Tree (Tree(..))
import Auxiliary.Logic (Logic(..))
import Data.Generics (Data, everywhere, mkT)

{-# INLINE updateInt #-}
updateInt :: (Data a) => a -> a
updateInt = everywhere (mkT (\n -> if odd n then n+1 else (n-1 :: Int)))

-- updateString :: Data a => (Char -> Char) -> a -> a
-- updateString f = everywhere (mkT f)
