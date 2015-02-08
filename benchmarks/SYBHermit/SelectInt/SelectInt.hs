{-# LANGUAGE RankNTypes #-}
module SYBHermit.SelectInt.SelectInt where

import Data.Generics
import TreeDatatype

selectInt :: Data a => (forall r. (r -> r -> r) -> GenericQ r -> GenericQ r) -> a -> [Int]
selectInt ething = ething (++) (mkQ [] f) where
  f :: Int -> [Int]
  f x = [x]

everythingR :: (r -> r -> r) -> GenericQ r -> GenericQ r
everythingR k f x = foldr k (f x) (gmapQ (everythingR k f) x)

everythingQl :: (r -> r -> r) -> GenericQ r -> GenericQ r
everythingQl k f x = gmapQl k (f x) (everythingQl k f) x

everythingQr :: (r -> r -> r) -> GenericQ r -> GenericQ r
everythingQr k f x = gmapQr k (f x) (everythingQr k f) x

selectInt_acc :: Data a => a -> [Int]
-- selectInt_acc x = everything (.) (mkQ id (:)) x []
selectInt_acc x = everythingAcc (mkQ id (:)) x []

everythingAcc :: GenericQ (r -> r) -> GenericQ (r -> r)
everythingAcc f x acc =
  gmapQr ($) (f x acc) (everythingAcc f) x

{-
r' = r -> r
gmapQr :: forall r r'. (r' -> r -> r) -> r -> (forall d. Data d => d -> r') -> a -> r
  gmapQr o r0 f x0 = unQr (gfoldl k (const (Qr id)) x0) r0
    where
      k :: Data d => Qr r (d->b) -> d -> Qr r b
      k (Qr c) x = Qr (\r -> c (f x `o` r))
-}
