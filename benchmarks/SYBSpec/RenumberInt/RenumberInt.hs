module SYBSpec.RenumberInt.RenumberInt where

import Data.Generics
import Control.Monad.State.Strict

getUnique :: Int -> State Int Int
getUnique _ = do
    u <- get
    modify (+1)
    return u

{-# INLINE renumberInt #-}
renumberInt :: Data a => a -> State Int a
renumberInt = everywhereM (mkM getUnique)
