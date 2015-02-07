{-# LANGUAGE CPP #-}
module HERMIT.Optimization.SYB.Prelude
    ( -- re-export all the imports below, so inlining info is available
      -- during compilation of the target program
      module Data.Function
    , module GHC.Base
    , module GHC.Fingerprint
    , module GHC.Fingerprint.Type
    , module GHC.Prim
    , module GHC.Word
    , append
    ) where

import Data.Function (fix)
import GHC.Base((++))
import GHC.Fingerprint (fingerprintFingerprints, Fingerprint(..))
import GHC.Fingerprint.Type
import GHC.Prim -- eqWord#
import GHC.Word

{-# RULES "append"       [~]           (++)        = append #-}
{-# RULES "append-left"  [~] forall x. append [] x = x      #-}
{-# RULES "append-right" [~] forall x. append x [] = x      #-}
{-# RULES "unappend"     [~]           append      = (++)   #-}

{-# INLINABLE append #-}
append :: [a] -> [a] -> [a]
append []     xs = xs
append (y:ys) xs = y : append ys xs
