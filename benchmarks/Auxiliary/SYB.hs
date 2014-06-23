{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE FlexibleInstances     #-}

{-# OPTIONS_GHC -fexpose-all-unfoldings #-}
module Auxiliary.SYB where

import Auxiliary.Tree (Tree(..))
import Auxiliary.Logic (Logic(..))
import Data.Generics (Data(..))

deriving instance Data (Tree Int)
deriving instance Data Logic
