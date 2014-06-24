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
                   ghcFlags :: String
                 } deriving (Eq, Ord, Show)

test :: Library -> TestName -> Datatype -> Test
test l t d = Test l t d ""

htest :: Library -> TestName -> Datatype -> String -> Test
htest l t d f = Test l t d hermitFlags
    where hermitFlags = unwords ["-dcore-lint"             -- HERMIT sanity check
                                ,"-fsimple-list-literals"  -- HERMIT
                                ,"-fexpose-all-unfoldings" -- optimization diverges without
                                ,"-fplugin=HERMIT.Optimization.SYB"
                                ,modPrefix ++ f            -- function targetted by optimization
                                ]
          modPrefix = "-fplugin-opt=HERMIT.Optimization.SYB:" ++ show l ++ "." ++ show t ++ ".Main:"


stest :: Library -> TestName -> Datatype -> String -> Test
stest l t d m = Test l t d flags
  where flags = unwords ["-fplugin=InlineGmap"
                        ,"-fplugin-opt=InlineGmap:" ++ m ++ ":\"+6 +6 +6 +6\""
                        ,"-fexpose-all-unfoldings"
                        ]

data Datatype = Tree    -- Labelled binary trees
              | Logic   -- Logic expressions
              | Nat     -- Peano naturals
              | List    -- Haskell lists
              | WTree   -- Weighted Tree from GPBench
                deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

allTests, handTests, sybTests, hermitSybTests, sybSpecTests :: [Test]
handTests = [ test Hand Eq     Tree
            , test Hand Map    Tree
            , test Hand Read   Tree
            , test Hand Show   Tree
--            , test Hand Update Tree
            , test Hand Enum   Tree
            , test Hand Enum   Nat
            , test Hand Decode Tree
            , test Hand Eq     Logic
            , test Hand Read   Logic
            , test Hand Show   Logic
            , test Hand Update Logic
            , test Hand Enum   Logic
            , test Hand Decode Logic
            , test Hand RmWeights WTree
            , test Hand SelectInt WTree
            , test Hand RenumberInt Tree
            , test Hand RenumberInt Logic]

sybTests = [ test SYB Eq     Tree
           , test SYB Map    Tree
           , test SYB Read   Tree
           , test SYB Show   Tree
--           , test SYB Update Tree
           , test SYB Enum   Tree
           , test SYB Eq     Logic
           , test SYB Read   Logic
           , test SYB Show   Logic
           , test SYB Update Logic
           , test SYB Enum   Logic
           , test SYB RmWeights WTree
           , test SYB SelectInt WTree
           , test SYB RenumberInt Tree
           , test SYB RenumberInt Logic
           ]

hermitSybTests =
    [ htest SYBHermit Map         Tree "incTree"
--    , htest SYBHermit Update      Tree ""
    , htest SYBHermit Update      Logic "updateString"
    , htest SYBHermit RmWeights   WTree "mainWTree"
    , htest SYBHermit SelectInt   WTree "mainWTree"
    , htest SYBHermit RenumberInt Tree  "mainTree"
    , htest SYBHermit RenumberInt Logic "mainLogic"
    ]

sybSpecTests =
    [ stest SYBSpec Map         Tree  "SYBSpec.Map.Main"
    , stest SYBSpec Update      Logic "SYBSpec.RmWeights.Update"
    , stest SYBSpec RmWeights   WTree "SYBSpec.RmWeights.RmWeights"
    , stest SYBSpec SelectInt   WTree "SYBSpec.RmWeights.SelectInt"
    , stest SYBSpec RenumberInt Tree  "SYBSpec.RenumberInt.RenumberInt"
    , stest SYBSpec RenumberInt Logic "SYBSpec.RenumberInt.RenumberInt"
    ]


allTests = handTests ++ sybTests ++ sybSpecTests ++ hermitSybTests

tests = [ t | t <- allTests
        , lib t `elem` [Hand,SYB,SYBSpec]
        -- , testName t `elem` [Map, RenumberInt, Update, RmWeights, SelectInt]
        , testName t `elem` [Map]
        ]
