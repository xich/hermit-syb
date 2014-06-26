{-# LANGUAGE BangPatterns, TemplateHaskell #-}
module Auxiliary.Auxiliary (
    test, apply, applyM,
    (|||), diag, intersperse,
    Bit, encodeInt, decodeInt, encodeString, decodeString,
    allEqual, seqHsModule
  ) where

import System.CPUTime (getCPUTime)
import Data.Time.Clock
import Data.List (mapAccumR)

import Control.Monad ((>=>))

import Prelude hiding (($!))
import GHC.Real(Ratio((:%)))
import Language.Haskell.Syntax

test :: IO () -> IO ()
test t = do
          t1w <- getCurrentTime
          t1c <- getCPUTime
          t
          t2c <- getCPUTime
          t2w <- getCurrentTime
          let diffc = fromInteger (t2c - t1c) / (1000000000 :: Double)
              diffw = fromRational (toRational (diffUTCTime t2w t1w)) * 1000
          putStrLn ("\t" ++ show diffc ++ "\t" ++ show diffw)

{-# INLINE apply #-}
apply :: Int -> (a -> a) -> a -> a
apply 1 f = f
apply n f | n > 1 = f . apply (pred n) f

{-# INLINE applyM #-}
applyM :: Monad m => Int -> (a -> m a) -> a -> m a
applyM 1 k = k
applyM n k | n > 1 = k >=> applyM (pred n) k

-----------------------------------------------------------------------------
-- Utility functions for Enum
-----------------------------------------------------------------------------

infixr 5 |||

-- | Interleave elements from two lists. Similar to (++), but swap left and
-- right arguments on every recursive application.
--
-- From Mark Jones' talk at AFP2008
(|||) :: [a] -> [a] -> [a]
[]     ||| ys = ys
(x:xs) ||| ys = x : ys ||| xs

-- | Diagonalization of nested lists. Ensure that some elements from every
-- sublist will be included. Handles infinite sublists.
--
-- From Mark Jones' talk at AFP2008
diag :: [[a]] -> [a]
diag = concat . foldr skew [] . map (map (\x -> [x]))

skew :: [[a]] -> [[a]] -> [[a]]
skew []     ys = ys
skew (x:xs) ys = x : combine (++) xs ys

combine :: (a -> a -> a) -> [a] -> [a] -> [a]
combine _ xs     []     = xs
combine _ []     ys     = ys
combine f (x:xs) (y:ys) = f x y : combine f xs ys

intersperse :: [[a]] -> [a]
intersperse ls = let f :: [[a]] -> ([a], [[a]])
                     f [] = ([], [])
                     f ([]:ls) = f ls
                     f ((h:t):ls) = let (a,b) = f ls in (h:a,t:b)
                 in case f ls of
                   (l,[])  -> l
                   (l,lss) -> l ++ intersperse lss where

type Bit = Int
{-
encodeInt :: Int -> [Bit] -> [Bit]
encodeInt x | x < 0 || x > 255 = error "encodeInt: Int too large"
            | otherwise        = (++) [0,0,0,1,0,1,1,0] -- temp simplification

decodeInt :: [Bit] -> (Int, [Bit])
decodeInt l | length l < 8 = error "decodeInt: cannot decode"
            | otherwise    = let (h,t) = splitAt 8 l
                                 f a (b,n) = (b + a * 2 ^ n, succ n)
                             in (fst (foldr f (0,0) h), t)
-}
encodeInt :: Int -> [Bit] -> [Bit]
encodeInt _ = (:) 1 -- simplification

decodeInt :: [Bit] -> (Int, [Bit])
decodeInt (_:t) = (53, t) -- simplification
decodeInt []    = error "decodeInt: cannot decode"

encodeString :: String -> [Bit] -> [Bit]
encodeString _ = (:) 1 -- simplification

decodeString :: [Bit] -> (String, [Bit])
decodeString (_:t) = ("x7", t) -- simplification
decodeString []    = error "decodeString: cannot decode"

allEqual :: (Eq a) => [a] -> Bool
allEqual (h1:h2:[]) = h1 == h2
allEqual (h1:h2:t)  = h1 == h2 && allEqual (h2:t)
allEqual [x]        = error "allEqual"
allEqual []         = error "allEqual"

-----------------------------------------------

-- We don't want this associated to the right.
infixl 0 $!
($!) :: (a -> b) -> a -> b
f $! x = let !v = x in f v

seqString = seqList seqChar

seqList f [] = []
seqList f (x:xs) = (:) $! (f x) $! (seqList f xs)

seqChar c = c `seq` c

seqHsName (HsIdent s) = HsIdent $! (seqString s)
seqHsName (HsSymbol s) = HsSymbol $! (seqString s)

seqHsNames = seqList seqHsName

seqHsLiteral (HsChar c) = HsChar $! (seqChar c)
seqHsLiteral (HsString s) = HsString $! (seqString s)
seqHsLiteral (HsInt i) = HsInt $! (seqInteger i)
seqHsLiteral (HsFrac r) = HsFrac $! (seqRational r)
seqHsLiteral (HsCharPrim c) = HsCharPrim $! (seqChar c)
seqHsLiteral (HsStringPrim s) = HsStringPrim $! (seqString s)
seqHsLiteral (HsIntPrim i) = HsIntPrim $! (seqInteger i)
seqHsLiteral (HsFloatPrim r) = HsFloatPrim $! (seqRational r)
seqHsLiteral (HsDoublePrim r) = HsDoublePrim $! (seqRational r)

seqSrcLoc (SrcLoc s i1 i2) = SrcLoc $! (seqString s) $! (seqInt i1) $! (seqInt i2)
seqModule (Module s) = Module $! (seqString s)

seqBool b = b `seq` b
seqMaybeModule Nothing = Nothing
seqMaybeModule (Just m) = Just $! (seqModule m)
seqInt i = i `seq` i
seqInteger i = i `seq` i
seqRational (i1 :% i2) = (:%) $! (seqInteger i1) $! (seqInteger i2)

seqHsCNames = seqList seqHsCName
seqHsCName (HsVarName hsName) = HsVarName $! (seqHsName hsName)
seqHsCName (HsConName hsName) = HsConName $! (seqHsName hsName)

seqHsQNames = seqList seqHsQName
seqHsQName (Qual modul hsName) = Qual $! (seqModule modul) $! (seqHsName hsName)
seqHsQName (UnQual hsName) = UnQual $! (seqHsName hsName)
seqHsQName (Special hsSpecialCon) = Special $! (seqHsSpecialCon hsSpecialCon)

seqHsSpecialCon HsUnitCon = HsUnitCon
seqHsSpecialCon HsListCon = HsListCon
seqHsSpecialCon HsFunCon = HsFunCon
seqHsSpecialCon (HsTupleCon i) = HsTupleCon $! (seqInt i)
seqHsSpecialCon HsCons = HsCons

-----------
-- Module

seqHsModule (HsModule srcLoc modul maybeHsExportSpecs hsImportDecls hsDecls) = HsModule $! (seqSrcLoc srcLoc) $! (seqModule modul) $! (seqMaybeHsExportSpecs maybeHsExportSpecs) $! (seqHsImportDecls hsImportDecls) $! (seqHsDecls hsDecls)

seqMaybeHsExportSpecs Nothing = Nothing
seqMaybeHsExportSpecs (Just hsExportSpecs) = Just $! (seqList seqHsExportSpec hsExportSpecs)
seqHsExportSpecs = seqList seqHsExportSpec
seqHsExportSpec (HsEVar hsQName) = HsEVar $! (seqHsQName hsQName)
seqHsExportSpec (HsEAbs hsQName) = HsEAbs $! (seqHsQName hsQName)
seqHsExportSpec (HsEThingAll hsQName) = HsEThingAll $! (seqHsQName hsQName)
seqHsExportSpec (HsEThingWith hsQName hsCNames) = HsEThingWith $! (seqHsQName hsQName) $! (seqHsCNames hsCNames)
seqHsExportSpec (HsEModuleContents modul) = HsEModuleContents $! (seqModule modul)


seqHsImportSpecs = seqList seqHsImportSpec
seqHsImportSpec (HsIVar hsName) = HsIVar $! (seqHsName hsName)
seqHsImportSpec (HsIAbs hsName) = HsIAbs $! (seqHsName hsName)
seqHsImportSpec (HsIThingAll hsName) = HsIThingAll $! (seqHsName hsName)
seqHsImportSpec (HsIThingWith hsName hsCNames) = HsIThingWith $! (seqHsName hsName) $! (seqHsCNames hsCNames)

seqHsImportDecls = seqList seqHsImportDecl
seqHsImportDecl (HsImportDecl srcLoc modul bool maybeModule maybeBoolHsImportSpecs) = HsImportDecl $! (seqSrcLoc srcLoc) $! (seqModule modul) $! (seqBool bool) $! (seqMaybeModule maybeModule) $! (seqMaybeBoolHsImportSpecs maybeBoolHsImportSpecs)
seqMaybeBoolHsImportSpecs Nothing = Nothing
seqMaybeBoolHsImportSpecs (Just (bool, hsImportSpecs)) = let !l = seqBool bool
                                                             !r = seqHsImportSpecs hsImportSpecs
                                                         in Just $! (l, r)

-----------------
-- Decl

seqHsDecls = seqList seqHsDecl
seqHsDecl (HsTypeDecl srcLoc hsName hsNames hsType) = HsTypeDecl $! (seqSrcLoc srcLoc) $! (seqHsName hsName) $! (seqHsNames hsNames) $! (seqHsType hsType)
seqHsDecl (HsDataDecl srcLoc hsContext hsName hsNames hsConDecls hsQNames) = HsDataDecl $! (seqSrcLoc srcLoc) $! (seqHsContext hsContext) $! (seqHsName hsName) $! (seqHsNames hsNames) $! (seqHsConDecls hsConDecls) $! (seqHsQNames hsQNames)
seqHsDecl (HsInfixDecl srcLoc hsAssoc int hsOps) = HsInfixDecl $! (seqSrcLoc srcLoc) $! (seqHsAssoc hsAssoc) $! (seqInt int) $! (seqHsOps hsOps)
seqHsDecl (HsNewTypeDecl srcLoc hsContext hsName hsNames hsConDecl hsQNames) = HsNewTypeDecl $! (seqSrcLoc srcLoc) $! (seqHsContext hsContext) $! (seqHsName hsName) $! (seqHsNames hsNames) $! (seqHsConDecl hsConDecl) $! (seqHsQNames hsQNames)
seqHsDecl (HsClassDecl srcLoc hsContext hsName hsNames hsDecls) = HsClassDecl $! (seqSrcLoc srcLoc) $! (seqHsContext hsContext) $! (seqHsName hsName) $! (seqHsNames hsNames) $! (seqHsDecls hsDecls)
seqHsDecl (HsInstDecl srcLoc hsContext hsQName hsTypes hsDecls) = HsInstDecl $! (seqSrcLoc srcLoc) $! (seqHsContext hsContext) $! (seqHsQName hsQName) $! (seqHsTypes hsTypes) $! (seqHsDecls hsDecls)
seqHsDecl (HsDefaultDecl srcLoc hsTypes) = HsDefaultDecl $! (seqSrcLoc srcLoc) $! (seqHsTypes hsTypes)
seqHsDecl (HsTypeSig srcLoc hsNames hsQualType) = HsTypeSig $! (seqSrcLoc srcLoc) $! (seqHsNames hsNames) $! (seqHsQualType hsQualType)
seqHsDecl (HsFunBind hsMatchs) = HsFunBind $! (seqHsMatchs hsMatchs)
seqHsDecl (HsPatBind srcLoc hsPat hsRhs hsDecls) = HsPatBind $! (seqSrcLoc srcLoc) $! (seqHsPat hsPat) $! (seqHsRhs hsRhs) $! (seqHsDecls hsDecls)
seqHsDecl (HsForeignImport srcLoc string1 hsSafety string2 hsName hsType) = HsForeignImport $! (seqSrcLoc srcLoc) $! (seqString string1) $! (seqHsSafety hsSafety) $! (seqString string2) $! (seqHsName hsName) $! (seqHsType hsType)
seqHsDecl (HsForeignExport srcLoc string1 string2 hsName hsType) = HsForeignExport $! (seqSrcLoc srcLoc) $! (seqString string1) $! (seqString string2) $! (seqHsName hsName) $! (seqHsType hsType)

seqHsConDecls = seqList seqHsConDecl
seqHsConDecl (HsConDecl srcLoc hsName hsBangTypes) = HsConDecl $! (seqSrcLoc srcLoc) $! (seqHsName hsName) $! (seqHsBangTypes hsBangTypes)
seqHsConDecl (HsRecDecl srcLoc hsName hsNamesHsBangTypes) = HsRecDecl $! (seqSrcLoc srcLoc) $! (seqHsName hsName) $! (seqHsNamesHsBangTypes hsNamesHsBangTypes)

seqHsNamesHsBangTypes = seqList seqHsNamesHsBangType
seqHsNamesHsBangType (hsNames, hsBangType) = (,) $! (seqHsNames hsNames) $! (seqHsBangType hsBangType)
seqHsBangTypes = seqList seqHsBangType
seqHsBangType (HsBangedTy hsType) = HsBangedTy $! (seqHsType hsType)
seqHsBangType (HsUnBangedTy hsType) = HsUnBangedTy $! (seqHsType hsType)
seqHsAssoc HsAssocNone = HsAssocNone
seqHsAssoc HsAssocLeft = HsAssocLeft
seqHsAssoc HsAssocRight = HsAssocRight

seqHsOps = seqList seqHsOp
seqHsOp (HsVarOp hsName) = HsVarOp $! (seqHsName hsName)
seqHsOp (HsConOp hsName) = HsConOp $! (seqHsName hsName)

seqHsMatchs = seqList seqHsMatch
seqHsMatch (HsMatch srcLoc hsName hsPats hsRhs hsDecls) = HsMatch $! (seqSrcLoc srcLoc) $! (seqHsName hsName) $! (seqHsPats hsPats) $! (seqHsRhs hsRhs) $! (seqHsDecls hsDecls)

seqHsRhs (HsUnGuardedRhs hsExp) = HsUnGuardedRhs $! (seqHsExp hsExp)
seqHsRhs (HsGuardedRhss hsGuardedRhss) = HsGuardedRhss $! (seqHsGuardedRhss hsGuardedRhss)

seqHsGuardedRhss = seqList seqHsGuardedRhs
seqHsGuardedRhs (HsGuardedRhs srcLoc hsExp1 hsExp2) = HsGuardedRhs $! (seqSrcLoc srcLoc) $! (seqHsExp hsExp1) $! (seqHsExp hsExp2)
seqHsSafety HsSafe = HsSafe
seqHsSafety HsUnsafe = HsUnsafe

-----------------
-- Types

seqHsTypes = seqList seqHsType
seqHsType (HsTyFun hsType1 hsType2) = HsTyFun $! (seqHsType hsType1) $! (seqHsType hsType2)
seqHsType (HsTyTuple hsTypes) = HsTyTuple $! (seqHsTypes hsTypes)
seqHsType (HsTyApp hsType1 hsType2) = HsTyApp $! (seqHsType hsType1) $! (seqHsType hsType2)
seqHsType (HsTyVar hsName) = HsTyVar $! (seqHsName hsName)
seqHsType (HsTyCon hsQName) = HsTyCon $! (seqHsQName hsQName)

seqHsQualType (HsQualType hsContext hsType) = HsQualType $! (seqHsContext hsContext) $! (seqHsType hsType)
seqHsContext hsAssts = seqList seqHsAsst hsAssts
seqHsAsst (hsQName, hsTypes) = (,) $! (seqHsQName hsQName) $! (seqHsTypes hsTypes)

---------------
-- Patterns

seqHsPats = seqList seqHsPat
seqHsPat (HsPVar hsName) = HsPVar $! (seqHsName hsName)
seqHsPat (HsPLit hsLiteral) = HsPLit $! (seqHsLiteral hsLiteral)
seqHsPat (HsPNeg hsPat) = HsPNeg $! (seqHsPat hsPat)
seqHsPat (HsPInfixApp hsPat1 hsQName hsPat2) = HsPInfixApp $! (seqHsPat hsPat1) $! (seqHsQName hsQName) $! (seqHsPat hsPat2)
seqHsPat (HsPApp hsQName hsPats) = HsPApp $! (seqHsQName hsQName) $! (seqHsPats hsPats)
seqHsPat (HsPTuple hsPats) = HsPTuple $! (seqHsPats hsPats)
seqHsPat (HsPList hsPats) = HsPList $! (seqHsPats hsPats)
seqHsPat (HsPParen hsPat) = HsPParen $! (seqHsPat hsPat)
seqHsPat (HsPRec hsQName hsPatFields) = HsPRec $! (seqHsQName hsQName) $! (seqHsPatFields hsPatFields)
seqHsPat (HsPAsPat hsName hsPat) = HsPAsPat $! (seqHsName hsName) $! (seqHsPat hsPat)
seqHsPat (HsPWildCard) = HsPWildCard
seqHsPat (HsPIrrPat hsPat) = HsPIrrPat $! (seqHsPat hsPat)

seqHsPatFields = seqList seqHsPatField
seqHsPatField (HsPFieldPat hsQName hsPat) = HsPFieldPat $! (seqHsQName hsQName) $! (seqHsPat hsPat)

----------------
-- Exp

seqHsExps = seqList seqHsExp
seqHsExp (HsVar hsQName) = HsVar $! (seqHsQName hsQName)
seqHsExp (HsCon hsQName) = HsCon $! (seqHsQName hsQName)
seqHsExp (HsLit hsLiteral) = HsLit $! (seqHsLiteral hsLiteral)
seqHsExp (HsInfixApp hsExp1 hsQOp hsExp2) = HsInfixApp $! (seqHsExp hsExp1) $! (seqHsQOp hsQOp) $! (seqHsExp hsExp2)
seqHsExp (HsApp hsExp1 hsExp2) = HsApp $! (seqHsExp hsExp1) $! (seqHsExp hsExp2)
seqHsExp (HsNegApp hsExp) = HsNegApp $! (seqHsExp hsExp)
seqHsExp (HsLambda srcLoc hsPats hsExp) = HsLambda $! (seqSrcLoc srcLoc) $! (seqHsPats hsPats) $! (seqHsExp hsExp)
seqHsExp (HsLet hsDecls hsExp) = HsLet $! (seqHsDecls hsDecls) $! (seqHsExp hsExp)
seqHsExp (HsIf hsExp1 hsExp2 hsExp3) = HsIf $! (seqHsExp hsExp1) $! (seqHsExp hsExp2) $! (seqHsExp hsExp3)
seqHsExp (HsCase hsExp hsAlts) = HsCase $! (seqHsExp hsExp) $! (seqHsAlts hsAlts)
seqHsExp (HsDo hsStmts) = HsDo $! (seqHsStmts hsStmts)
seqHsExp (HsTuple hsExps) = HsTuple $! (seqHsExps hsExps)
seqHsExp (HsList hsExps) = HsList $! (seqHsExps hsExps)
seqHsExp (HsParen hsExp) = HsParen $! (seqHsExp hsExp)
seqHsExp (HsLeftSection hsExp hsQOp) = HsLeftSection $! (seqHsExp hsExp) $! (seqHsQOp hsQOp)
seqHsExp (HsRightSection hsQOp hsExp) = HsRightSection $! (seqHsQOp hsQOp) $! (seqHsExp hsExp)
seqHsExp (HsRecConstr hsQName hsFieldUpdates) = HsRecConstr $! (seqHsQName hsQName) $! (seqHsFieldUpdates hsFieldUpdates)
seqHsExp (HsRecUpdate hsExp hsFieldUpdates) = HsRecUpdate $! (seqHsExp hsExp) $! (seqHsFieldUpdates hsFieldUpdates)
seqHsExp (HsEnumFrom hsExp) = HsEnumFrom $! (seqHsExp hsExp)
seqHsExp (HsEnumFromTo hsExp1 hsExp2) = HsEnumFromTo $! (seqHsExp hsExp1) $! (seqHsExp hsExp2)
seqHsExp (HsEnumFromThen hsExp1 hsExp2) = HsEnumFromThen $! (seqHsExp hsExp1) $! (seqHsExp hsExp2)
seqHsExp (HsEnumFromThenTo hsExp1 hsExp2 hsExp3) = HsEnumFromThenTo $! (seqHsExp hsExp1) $! (seqHsExp hsExp2) $! (seqHsExp hsExp3)
seqHsExp (HsListComp hsExp hsStmts) = HsListComp $! (seqHsExp hsExp) $! (seqHsStmts hsStmts)
seqHsExp (HsExpTypeSig srcLoc hsExp hsQualType) = HsExpTypeSig $! (seqSrcLoc srcLoc) $! (seqHsExp hsExp) $! (seqHsQualType hsQualType)
seqHsExp (HsAsPat hsName hsExp) = HsAsPat $! (seqHsName hsName) $! (seqHsExp hsExp)
seqHsExp (HsWildCard) = HsWildCard
seqHsExp (HsIrrPat hsExp) = HsIrrPat $! (seqHsExp hsExp)

seqHsQOp (HsQVarOp hsQName) = HsQVarOp $! (seqHsQName hsQName)
seqHsQOp (HsQConOp hsQName) = HsQConOp $! (seqHsQName hsQName)

seqHsAlts = seqList seqHsAlt
seqHsAlt (HsAlt srcLoc hsPat hsGuardedAlts hsDecls) = HsAlt $! (seqSrcLoc srcLoc) $! (seqHsPat hsPat) $! (seqHsGuardedAlts hsGuardedAlts) $! (seqHsDecls hsDecls)
seqHsGuardedAlts (HsUnGuardedAlt hsExp) = HsUnGuardedAlt $! (seqHsExp hsExp)
seqHsGuardedAlts (HsGuardedAlts hsGuardedAltList) = HsGuardedAlts $! (seqHsGuardedAltList hsGuardedAltList)
seqHsGuardedAltList = seqList seqHsGuardedAlt
seqHsGuardedAlt (HsGuardedAlt srcLoc hsExp1 hsExp2) = HsGuardedAlt $! (seqSrcLoc srcLoc) $! (seqHsExp hsExp1) $! (seqHsExp hsExp2)

seqHsStmts = seqList seqHsStmt
seqHsStmt (HsGenerator srcLoc hsPat hsExp) = HsGenerator $! (seqSrcLoc srcLoc) $! (seqHsPat hsPat) $! (seqHsExp hsExp)
seqHsStmt (HsQualifier hsExp) = HsQualifier $! (seqHsExp hsExp)
seqHsStmt (HsLetStmt hsDecls) = HsLetStmt $! (seqHsDecls hsDecls)

seqHsFieldUpdates = seqList seqHsFieldUpdate
seqHsFieldUpdate (HsFieldUpdate hsQName hsExp) = HsFieldUpdate $! (seqHsQName hsQName) $! (seqHsExp hsExp)
