{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module SYBSpec.SelectInt.SelectInt where

import Data.Generics
import TreeDatatype

{-# INLINE selectInt_acc #-}
selectInt_acc :: Data a => a -> [Int]
-- selectInt_acc x = everything (.) (mkQ id (:)) x []
selectInt_acc x = everythingAcc (mkQ id (:)) x []

{-# INLINE everythingAcc #-}
everythingAcc :: forall r. GenericQ (r -> r) -> GenericQ (r -> r)
everythingAcc f = go where
  go :: GenericQ (r -> r)
  go x acc = gmapQr ($) (f x acc) go x
