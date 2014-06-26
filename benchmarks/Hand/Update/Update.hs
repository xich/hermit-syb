module Hand.Update.Update where

import Auxiliary.Tree (Tree(..))
import Auxiliary.Logic (Logic(..))

import GHC.Real (Ratio((:%)))
import Language.Haskell.Syntax

updateTree :: Tree Int -> Tree Int
updateTree Leaf        = Leaf
updateTree (Bin n l r) | odd n     = (Bin (n+1)) (updateTree l) (updateTree r)
                       | otherwise = (Bin (n-1)) (updateTree l) (updateTree r)

updateLogic :: (Char -> Char) -> Logic -> Logic
updateLogic f l = go l
  where
    go (Var s i)   = Var (map f s) i
    go (Impl  p q) = Impl  (go p) (go q)
    go (Equiv p q) = Equiv (go p) (go q)
    go (Conj  p q) = Conj  (go p) (go q)
    go (Disj  p q) = Disj  (go p) (go q)
    go (Not p)     = Not (go p)
    go T           = T
    go F           = F

-------------------------------

--- the logic
-- use this definition to prevent benefits from static-arg'd map
updateString [] = []
updateString (_:ys) = 'y' : updateString ys

updateChar _ = 'y'
--- end logic

updateHsName (HsIdent s) = HsIdent (updateString s)
updateHsName (HsSymbol s) = HsSymbol (updateString s)

updateHsNames = Prelude.map updateHsName

updateHsLiteral (HsChar c) = HsChar (updateChar c)
updateHsLiteral (HsString s) = HsString (updateString s)
updateHsLiteral (HsInt i) = HsInt (updateInteger i)
updateHsLiteral (HsFrac r) = HsFrac (updateRational r)
updateHsLiteral (HsCharPrim c) = HsCharPrim (updateChar c)
updateHsLiteral (HsStringPrim s) = HsStringPrim (updateString s)
updateHsLiteral (HsIntPrim i) = HsIntPrim (updateInteger i)
updateHsLiteral (HsFloatPrim r) = HsFloatPrim (updateRational r)
updateHsLiteral (HsDoublePrim r) = HsDoublePrim (updateRational r)

updateSrcLoc (SrcLoc s i1 i2) = SrcLoc (updateString s) (updateInt i1) (updateInt i2)
updateModule (Module s) = Module (updateString s)

updateBool b = b
updateMaybeModule Nothing = Nothing
updateMaybeModule (Just m) = Just (updateModule m)
updateInt i = i
updateInteger i = i
updateRational (i1 :% i2) = (updateInteger i1) :% (updateInteger i2)

updateHsCNames = Prelude.map updateHsCName
updateHsCName (HsVarName hsName) = HsVarName (updateHsName hsName)
updateHsCName (HsConName hsName) = HsConName (updateHsName hsName)

updateHsQNames = Prelude.map updateHsQName
updateHsQName (Qual modul hsName) = Qual (updateModule modul) (updateHsName hsName)
updateHsQName (UnQual hsName) = UnQual (updateHsName hsName)
updateHsQName (Special hsSpecialCon) = Special (updateHsSpecialCon hsSpecialCon)

updateHsSpecialCon HsUnitCon = HsUnitCon
updateHsSpecialCon HsListCon = HsListCon
updateHsSpecialCon HsFunCon = HsFunCon
updateHsSpecialCon (HsTupleCon i) = HsTupleCon (updateInt i)
updateHsSpecialCon HsCons = HsCons

-----------
-- Module

updateHsModule (HsModule srcLoc modul maybeHsExportSpecs hsImportDecls hsDecls) = HsModule (updateSrcLoc srcLoc) (updateModule modul) (updateMaybeHsExportSpecs maybeHsExportSpecs) (updateHsImportDecls hsImportDecls) (updateHsDecls hsDecls)

updateMaybeHsExportSpecs Nothing = Nothing
updateMaybeHsExportSpecs (Just hsExportSpecs) = Just (Prelude.map updateHsExportSpec hsExportSpecs)
updateHsExportSpecs = Prelude.map updateHsExportSpec
updateHsExportSpec (HsEVar hsQName) = HsEVar (updateHsQName hsQName)
updateHsExportSpec (HsEAbs hsQName) = HsEAbs (updateHsQName hsQName)
updateHsExportSpec (HsEThingAll hsQName) = HsEThingAll (updateHsQName hsQName)
updateHsExportSpec (HsEThingWith hsQName hsCNames) = HsEThingWith (updateHsQName hsQName) (updateHsCNames hsCNames)
updateHsExportSpec (HsEModuleContents modul) = HsEModuleContents (updateModule modul)


updateHsImportSpecs = Prelude.map updateHsImportSpec
updateHsImportSpec (HsIVar hsName) = HsIVar (updateHsName hsName)
updateHsImportSpec (HsIAbs hsName) = HsIAbs (updateHsName hsName)
updateHsImportSpec (HsIThingAll hsName) = HsIThingAll (updateHsName hsName)
updateHsImportSpec (HsIThingWith hsName hsCNames) = HsIThingWith (updateHsName hsName) (updateHsCNames hsCNames)

updateHsImportDecls = Prelude.map updateHsImportDecl
updateHsImportDecl (HsImportDecl srcLoc modul bool maybeModule maybeBoolHsImportSpecs) = HsImportDecl (updateSrcLoc srcLoc) (updateModule modul) (updateBool bool) (updateMaybeModule maybeModule) (updateMaybeBoolHsImportSpecs maybeBoolHsImportSpecs)
updateMaybeBoolHsImportSpecs Nothing = Nothing
updateMaybeBoolHsImportSpecs (Just (bool, hsImportSpecs)) = Just (updateBool bool, updateHsImportSpecs hsImportSpecs)

-----------------
-- Decl

updateHsDecls = Prelude.map updateHsDecl
updateHsDecl (HsTypeDecl srcLoc hsName hsNames hsType) = HsTypeDecl (updateSrcLoc srcLoc) (updateHsName hsName) (updateHsNames hsNames) (updateHsType hsType)
updateHsDecl (HsDataDecl srcLoc hsContext hsName hsNames hsConDecls hsQNames) = HsDataDecl (updateSrcLoc srcLoc) (updateHsContext hsContext) (updateHsName hsName) (updateHsNames hsNames) (updateHsConDecls hsConDecls) (updateHsQNames hsQNames)
updateHsDecl (HsInfixDecl srcLoc hsAssoc int hsOps) = HsInfixDecl (updateSrcLoc srcLoc) (updateHsAssoc hsAssoc) (updateInt int) (updateHsOps hsOps)
updateHsDecl (HsNewTypeDecl srcLoc hsContext hsName hsNames hsConDecl hsQNames) = HsNewTypeDecl (updateSrcLoc srcLoc) (updateHsContext hsContext) (updateHsName hsName) (updateHsNames hsNames) (updateHsConDecl hsConDecl) (updateHsQNames hsQNames)
updateHsDecl (HsClassDecl srcLoc hsContext hsName hsNames hsDecls) = HsClassDecl (updateSrcLoc srcLoc) (updateHsContext hsContext) (updateHsName hsName) (updateHsNames hsNames) (updateHsDecls hsDecls)
updateHsDecl (HsInstDecl srcLoc hsContext hsQName hsTypes hsDecls) = HsInstDecl (updateSrcLoc srcLoc) (updateHsContext hsContext) (updateHsQName hsQName) (updateHsTypes hsTypes) (updateHsDecls hsDecls)
updateHsDecl (HsDefaultDecl srcLoc hsTypes) = HsDefaultDecl (updateSrcLoc srcLoc) (updateHsTypes hsTypes)
updateHsDecl (HsTypeSig srcLoc hsNames hsQualType) = HsTypeSig (updateSrcLoc srcLoc) (updateHsNames hsNames) (updateHsQualType hsQualType)
updateHsDecl (HsFunBind hsMatchs) = HsFunBind (updateHsMatchs hsMatchs)
updateHsDecl (HsPatBind srcLoc hsPat hsRhs hsDecls) = HsPatBind (updateSrcLoc srcLoc) (updateHsPat hsPat) (updateHsRhs hsRhs) (updateHsDecls hsDecls)
updateHsDecl (HsForeignImport srcLoc string1 hsSafety string2 hsName hsType) = HsForeignImport (updateSrcLoc srcLoc) (updateString string1) (updateHsSafety hsSafety) (updateString string2) (updateHsName hsName) (updateHsType hsType)
updateHsDecl (HsForeignExport srcLoc string1 string2 hsName hsType) = HsForeignExport (updateSrcLoc srcLoc) (updateString string1) (updateString string2) (updateHsName hsName) (updateHsType hsType)

updateHsConDecls = Prelude.map updateHsConDecl
updateHsConDecl (HsConDecl srcLoc hsName hsBangTypes) = HsConDecl (updateSrcLoc srcLoc) (updateHsName hsName) (updateHsBangTypes hsBangTypes)
updateHsConDecl (HsRecDecl srcLoc hsName hsNamesHsBangTypes) = HsRecDecl (updateSrcLoc srcLoc) (updateHsName hsName) (updateHsNamesHsBangTypes hsNamesHsBangTypes)

updateHsNamesHsBangTypes = Prelude.map updateHsNamesHsBangType
updateHsNamesHsBangType (hsNames, hsBangType) = (,) (updateHsNames hsNames) (updateHsBangType hsBangType)
updateHsBangTypes = Prelude.map updateHsBangType
updateHsBangType (HsBangedTy hsType) = HsBangedTy (updateHsType hsType)
updateHsBangType (HsUnBangedTy hsType) = HsUnBangedTy (updateHsType hsType)
updateHsAssoc = id --data hsAssoc = HsAssocNone | HsAssocLeft | HsAssocRight

updateHsOps = Prelude.map updateHsOp
updateHsOp (HsVarOp hsName) = HsVarOp (updateHsName hsName)
updateHsOp (HsConOp hsName) = HsConOp (updateHsName hsName)

updateHsMatchs = Prelude.map updateHsMatch
updateHsMatch (HsMatch srcLoc hsName hsPats hsRhs hsDecls) = HsMatch (updateSrcLoc srcLoc) (updateHsName hsName) (updateHsPats hsPats) (updateHsRhs hsRhs) (updateHsDecls hsDecls)

updateHsRhs (HsUnGuardedRhs hsExp) = HsUnGuardedRhs (updateHsExp hsExp)
updateHsRhs (HsGuardedRhss hsGuardedRhss) = HsGuardedRhss (updateHsGuardedRhss hsGuardedRhss)

updateHsGuardedRhss = Prelude.map updateHsGuardedRhs
updateHsGuardedRhs (HsGuardedRhs srcLoc hsExp1 hsExp2) = HsGuardedRhs (updateSrcLoc srcLoc) (updateHsExp hsExp1) (updateHsExp hsExp2)
updateHsSafety = id --data HsSafety = HsSafe | HsUnsafe

-----------------
-- Types

updateHsTypes = Prelude.map updateHsType
updateHsType (HsTyFun hsType1 hsType2) = HsTyFun (updateHsType hsType1) (updateHsType hsType2)
updateHsType (HsTyTuple hsTypes) = HsTyTuple (updateHsTypes hsTypes)
updateHsType (HsTyApp hsType1 hsType2) = HsTyApp (updateHsType hsType1) (updateHsType hsType2)
updateHsType (HsTyVar hsName) = HsTyVar (updateHsName hsName)
updateHsType (HsTyCon hsQName) = HsTyCon (updateHsQName hsQName)

updateHsQualType (HsQualType hsContext hsType) = HsQualType (updateHsContext hsContext) (updateHsType hsType)
updateHsContext hsAssts = Prelude.map updateHsAsst hsAssts
updateHsAsst (hsQName, hsTypes) = (,) (updateHsQName hsQName) (updateHsTypes hsTypes)

---------------
-- Patterns

updateHsPats = Prelude.map updateHsPat
updateHsPat (HsPVar hsName) = HsPVar (updateHsName hsName)
updateHsPat (HsPLit hsLiteral) = HsPLit (updateHsLiteral hsLiteral)
updateHsPat (HsPNeg hsPat) = HsPNeg (updateHsPat hsPat)
updateHsPat (HsPInfixApp hsPat1 hsQName hsPat2) = HsPInfixApp (updateHsPat hsPat1) (updateHsQName hsQName) (updateHsPat hsPat2)
updateHsPat (HsPApp hsQName hsPats) = HsPApp (updateHsQName hsQName) (updateHsPats hsPats)
updateHsPat (HsPTuple hsPats) = HsPTuple (updateHsPats hsPats)
updateHsPat (HsPList hsPats) = HsPList (updateHsPats hsPats)
updateHsPat (HsPParen hsPat) = HsPParen (updateHsPat hsPat)
updateHsPat (HsPRec hsQName hsPatFields) = HsPRec (updateHsQName hsQName) (updateHsPatFields hsPatFields)
updateHsPat (HsPAsPat hsName hsPat) = HsPAsPat (updateHsName hsName) (updateHsPat hsPat)
updateHsPat (HsPWildCard) = HsPWildCard
updateHsPat (HsPIrrPat hsPat) = HsPIrrPat (updateHsPat hsPat)

updateHsPatFields = Prelude.map updateHsPatField
updateHsPatField (HsPFieldPat hsQName hsPat) = HsPFieldPat (updateHsQName hsQName) (updateHsPat hsPat)

----------------
-- Exp

updateHsExps = Prelude.map updateHsExp
updateHsExp (HsVar hsQName) = HsVar (updateHsQName hsQName)
updateHsExp (HsCon hsQName) = HsCon (updateHsQName hsQName)
updateHsExp (HsLit hsLiteral) = HsLit (updateHsLiteral hsLiteral)
updateHsExp (HsInfixApp hsExp1 hsQOp hsExp2) = HsInfixApp (updateHsExp hsExp1) (updateHsQOp hsQOp) (updateHsExp hsExp2)
updateHsExp (HsApp hsExp1 hsExp2) = HsApp (updateHsExp hsExp1) (updateHsExp hsExp2)
updateHsExp (HsNegApp hsExp) = HsNegApp (updateHsExp hsExp)
updateHsExp (HsLambda srcLoc hsPats hsExp) = HsLambda (updateSrcLoc srcLoc) (updateHsPats hsPats) (updateHsExp hsExp)
updateHsExp (HsLet hsDecls hsExp) = HsLet (updateHsDecls hsDecls) (updateHsExp hsExp)
updateHsExp (HsIf hsExp1 hsExp2 hsExp3) = HsIf (updateHsExp hsExp1) (updateHsExp hsExp2) (updateHsExp hsExp3)
updateHsExp (HsCase hsExp hsAlts) = HsCase (updateHsExp hsExp) (updateHsAlts hsAlts)
updateHsExp (HsDo hsStmts) = HsDo (updateHsStmts hsStmts)
updateHsExp (HsTuple hsExps) = HsTuple (updateHsExps hsExps)
updateHsExp (HsList hsExps) = HsList (updateHsExps hsExps)
updateHsExp (HsParen hsExp) = HsParen (updateHsExp hsExp)
updateHsExp (HsLeftSection hsExp hsQOp) = HsLeftSection (updateHsExp hsExp) (updateHsQOp hsQOp)
updateHsExp (HsRightSection hsQOp hsExp) = HsRightSection (updateHsQOp hsQOp) (updateHsExp hsExp)
updateHsExp (HsRecConstr hsQName hsFieldUpdates) = HsRecConstr (updateHsQName hsQName) (updateHsFieldUpdates hsFieldUpdates)
updateHsExp (HsRecUpdate hsExp hsFieldUpdates) = HsRecUpdate (updateHsExp hsExp) (updateHsFieldUpdates hsFieldUpdates)
updateHsExp (HsEnumFrom hsExp) = HsEnumFrom (updateHsExp hsExp)
updateHsExp (HsEnumFromTo hsExp1 hsExp2) = HsEnumFromTo (updateHsExp hsExp1) (updateHsExp hsExp2)
updateHsExp (HsEnumFromThen hsExp1 hsExp2) = HsEnumFromThen (updateHsExp hsExp1) (updateHsExp hsExp2)
updateHsExp (HsEnumFromThenTo hsExp1 hsExp2 hsExp3) = HsEnumFromThenTo (updateHsExp hsExp1) (updateHsExp hsExp2) (updateHsExp hsExp3)
updateHsExp (HsListComp hsExp hsStmts) = HsListComp (updateHsExp hsExp) (updateHsStmts hsStmts)
updateHsExp (HsExpTypeSig srcLoc hsExp hsQualType) = HsExpTypeSig (updateSrcLoc srcLoc) (updateHsExp hsExp) (updateHsQualType hsQualType)
updateHsExp (HsAsPat hsName hsExp) = HsAsPat (updateHsName hsName) (updateHsExp hsExp)
updateHsExp (HsWildCard) = HsWildCard
updateHsExp (HsIrrPat hsExp) = HsIrrPat (updateHsExp hsExp)

updateHsQOp (HsQVarOp hsQName) = HsQVarOp (updateHsQName hsQName)
updateHsQOp (HsQConOp hsQName) = HsQConOp (updateHsQName hsQName)

updateHsAlts = Prelude.map updateHsAlt
updateHsAlt (HsAlt srcLoc hsPat hsGuardedAlts hsDecls) = HsAlt (updateSrcLoc srcLoc) (updateHsPat hsPat) (updateHsGuardedAlts hsGuardedAlts) (updateHsDecls hsDecls)
updateHsGuardedAlts (HsUnGuardedAlt hsExp) = HsUnGuardedAlt (updateHsExp hsExp)
updateHsGuardedAlts (HsGuardedAlts hsGuardedAltList) = HsGuardedAlts (updateHsGuardedAltList hsGuardedAltList)
updateHsGuardedAltList = Prelude.map updateHsGuardedAlt
updateHsGuardedAlt (HsGuardedAlt srcLoc hsExp1 hsExp2) = HsGuardedAlt (updateSrcLoc srcLoc) (updateHsExp hsExp1) (updateHsExp hsExp2)

updateHsStmts = Prelude.map updateHsStmt
updateHsStmt (HsGenerator srcLoc hsPat hsExp) = HsGenerator (updateSrcLoc srcLoc) (updateHsPat hsPat) (updateHsExp hsExp)
updateHsStmt (HsQualifier hsExp) = HsQualifier (updateHsExp hsExp)
updateHsStmt (HsLetStmt hsDecls) = HsLetStmt (updateHsDecls hsDecls)

updateHsFieldUpdates = Prelude.map updateHsFieldUpdate
updateHsFieldUpdate (HsFieldUpdate hsQName hsExp) = HsFieldUpdate (updateHsQName hsQName) (updateHsExp hsExp)
