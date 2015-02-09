{-# LANGUAGE ScopedTypeVariables #-}

module Tests (Library(..), TestName(..), Test(..), Datatype(..), tests) where

import System.FilePath ((</>))

--------------------------------------------------------------------------------
-- Datatypes for representing libraries, tests, and datatypes tested on
--------------------------------------------------------------------------------

data Library = Hand          -- Handwritten code
             | SYB           -- Plain SYB
             | SYBHermit     -- SYB with Hermit optimisation
             | SYBSpec       -- SYB with GHC optimisation
                deriving (Eq, Ord, Show)

data TestName = Eq
              | Map
              | Read
              | Show
              | Update      -- Traversals
              | Arbitrary   -- QuickCheck's (1.2)
              | Enum
              | Decode
              | RmWeights   -- from GPBench's RmWeights
              | SelectInt   -- from GPBench's FoldTree
              | RenumberInt -- everywhereM
                 deriving (Eq, Ord, Show)

data Test = Test { lib :: Library,
                   testName :: TestName,
                   datatype :: Datatype,
                   ghcFlags :: String,
                   mainIs   :: Maybe String
                 } deriving (Eq, Ord, Show)

test :: Library -> TestName -> Datatype -> Maybe String -> Test
test l t d m = Test l t d "" m

htest :: Library -> TestName -> Datatype -> Maybe String -> String -> Test
htest l t d m f = Test l t d hermitFlags m
    where hermitFlags = unwords ["-dcore-lint"             -- HERMIT sanity check
                                ,"-fsimple-list-literals"  -- HERMIT
                                ,"-fexpose-all-unfoldings" -- optimization diverges without
                                ,"-fplugin=HERMIT.Optimization.SYB"
                                ,modPrefix ++ f            -- function targetted by optimization
                                ]
          modPrefix = "-fplugin-opt=HERMIT.Optimization.SYB:" ++ show l ++ "." ++ show t ++ ".Main:"


stest :: Library -> TestName -> Datatype -> Maybe String -> String -> Test
stest l t d m mod = Test l t d flags m
  where flags = unwords ["-fplugin=InlineGmap"
                        ,"-fplugin-opt=InlineGmap:" ++ mod ++ ":\"+6 +6 +6 +6\""
                        ,"-fexpose-all-unfoldings"
                        ]

data Datatype = Tree    -- Labelled binary trees
              | Logic   -- Logic expressions
              | Nat     -- Peano naturals
              | List    -- Haskell lists
              | WTree   -- Weighted Tree from GPBench
              | HsModule -- haskell-src
                deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

allTests, handTests, sybTests, hermitSybTests, sybSpecTests :: [Test]
handTests = [ test Hand Map    Tree Nothing
            , test Hand Read   Tree Nothing
            , test Hand Show   Tree Nothing
--            , test Hand Update Tree Nothing
            , test Hand Enum   Tree Nothing
            , test Hand Enum   Nat Nothing
            , test Hand Decode Tree Nothing
            , test Hand Read   Logic Nothing
            , test Hand Show   Logic Nothing
            , test Hand Update Logic Nothing
            , test Hand Enum   Logic Nothing
            , test Hand Decode Logic Nothing
            , test Hand RmWeights WTree Nothing
            , test Hand SelectInt WTree (Just "mainDumb")
            , test Hand SelectInt WTree (Just "mainSmart")
            , test Hand RenumberInt Tree Nothing
            , test Hand RenumberInt Logic Nothing
            , test Hand Update HsModule Nothing
            , test Hand Eq WTree Nothing
            ]

sybTests = [ test SYB Map    Tree Nothing
           , test SYB Read   Tree Nothing
           , test SYB Show   Tree Nothing
--           , test SYB Update Tree Nothing
           , test SYB Enum   Tree Nothing
           , test SYB Read   Logic Nothing
           , test SYB Show   Logic Nothing
           , test SYB Update Logic Nothing
           , test SYB Enum   Logic Nothing
           , test SYB RmWeights WTree Nothing
           , test SYB SelectInt WTree (Just "mainEverythingWTree")
           , test SYB SelectInt WTree (Just "mainEverythingRWTree")
           , test SYB SelectInt WTree (Just "mainEverythingQlWTree")
           , test SYB SelectInt WTree (Just "mainEverythingQrWTree")
           , test SYB RenumberInt Tree Nothing
           , test SYB RenumberInt Logic Nothing
           , test SYB Update HsModule Nothing
           , test SYB Eq WTree Nothing
           ]

hermitSybTests =
    [ htest SYBHermit Map         Tree Nothing "incTree"
    , htest SYBHermit RenumberInt Tree  Nothing "mainTree"
    , htest SYBHermit RenumberInt Logic Nothing "mainLogic"
    , htest SYBHermit RmWeights   WTree Nothing "mainWTree"
    , htest SYBHermit SelectInt   WTree (Just "mainEverythingWTree") "mainEverythingWTree"
    , htest SYBHermit SelectInt   WTree (Just "mainEverythingRWTree") "mainEverythingRWTree"
    , htest SYBHermit SelectInt   WTree (Just "mainEverythingQlWTree") "mainEverythingQlWTree"
    , htest SYBHermit SelectInt   WTree (Just "mainEverythingQrWTree") "mainEverythingQrWTree"
--    , htest SYBHermit Update      Tree Nothing "" -- see Map Tree
    , htest SYBHermit Update      Logic Nothing "updateStringLogic"
    , htest SYBHermit Update      HsModule Nothing "updateStringHsModule"
    , htest SYBHermit Eq          WTree Nothing "mainWTree"
    ]

sybSpecTests =
    [ stest SYBSpec Map         Tree  Nothing "SYBSpec.Map.Main"
    , stest SYBSpec Update      Logic Nothing "SYBSpec.Update.Main"
    , stest SYBSpec RmWeights   WTree Nothing "SYBSpec.RmWeights.Main"
    , stest SYBSpec SelectInt   WTree Nothing "SYBSpec.SelectInt.Main"
    , stest SYBSpec RenumberInt Tree  Nothing "SYBSpec.RenumberInt.Main"
    , stest SYBSpec RenumberInt Logic Nothing "SYBSpec.RenumberInt.Main"
    ]

allTests = handTests ++ sybTests ++ sybSpecTests ++ hermitSybTests

tests = [ t | t <- allTests
        , lib t `elem` [Hand,SYB,SYBHermit]
        , testName t `elem` [Eq] -- Map, RenumberInt, Update, RmWeights, SelectInt]
        , not (datatype t `elem` [HsModule])
        ]
