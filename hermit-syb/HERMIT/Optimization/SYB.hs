{-# LANGUAGE DoAndIfThenElse, CPP #-} -- why do we need this?
{-# LANGUAGE OverloadedStrings #-}
module HERMIT.Optimization.SYB where

import qualified Outputable (showSDocDebug)

import Control.Arrow
import Control.Monad
import Data.Generics (gshow)
import Data.List (intercalate, intersect, partition)
import Data.Maybe (maybe)
import Data.String (fromString)
import GHC.Fingerprint (Fingerprint(..), fingerprintFingerprints)

import qualified Data.Map as Map

import HERMIT.Context
import HERMIT.Core
import HERMIT.Dictionary
import HERMIT.External
import HERMIT.GHC hiding (display)
import HERMIT.Kernel
import HERMIT.Kure hiding (apply)
import HERMIT.Lemma
import HERMIT.Monad
import HERMIT.Name
import HERMIT.Plugin.Builder
import HERMIT.Plugin

import CoreUnfold (mkSimpleUnfolding)

plugin :: Plugin
plugin = hermitPlugin $ \ opts -> do
    let (opts', targets) = partition (`elem` ["interactive", "interactive-only"]) opts
    pass 0 $ do
        if "interactive-only" `elem` opts'
        then interactive exts []
        else do forM_ targets $ \ t -> do
                    liftIO $ putStrLn $ "optimizing: " ++ t
                    apply (Always $ "-- optimizing " ++ t) $ do
                        p <- rhsOfT (cmpString2Var t)
                        promoteR $ localPathR p
                                 $ repeatR optSYB
                                        >>> tryR (innermostR $ promoteExprR letrecSubstTrivialR)
                                        >>> tryR (anytdR $ promoteExprR $ foldRuleR UnsafeUsed "append") -- clean up
                                        >>> tryR simplifyR
                    display

    -- pass either interactive or interactive-only flags to dump into a shell at the end
    unless (null opts') $ after Simplify $ interactive exts []

optSimp :: RewriteH LCore
optSimp = anytdR (repeatR (promoteExprR (   unfoldRuleR UnsafeUsed (fromString "append")
                                         <+ unfoldRuleR UnsafeUsed (fromString "append-left")
                                         <+ unfoldRuleR UnsafeUsed (fromString "append-right")
                                         <+ castElimReflR
                                         <+ castElimSymPlusR
                                         <+ letElimR
                                         <+ letSubstType "*"
                                         <+ letSubstType "BOX"
                                         <+ evalFingerprintFingerprints
                                         <+ eqWordElim
                                         <+ tagToEnumElim
                                         <+ letSubstTrivialR
                                         <+ caseReduceR False)
                                            >>> traceR "SIMPLIFYING"))

optFloat :: RewriteH LCore
optFloat = anybuR (promoteExprR ((   memoFloatMemoLet
                                  <+ memoFloatMemoBind
                                  <+ memoFloatApp
                                  <+ memoFloatArg
                                  <+ memoFloatLam
                                  <+ memoFloatLet
                                  <+ memoFloatBind
                                  <+ memoFloatRecBind
                                  <+ memoFloatCase
                                  <+ memoFloatCast
                                  <+ memoFloatAlt)
                                     >>> traceR "FLOATING"))

optMemo :: RewriteH LCore
optMemo = smarttdR $ promoteExprR $ (>>) (   eliminatesType "Data"
                                          <+ eliminatesType "Typeable"
                                          <+ eliminatesType "Typeable1"
                                          <+ eliminatesType "TypeRep"
                                          <+ eliminatesType "TyCon"
                                          <+ eliminatesType "ID"
                                          <+ eliminatesType "Qr"
                                          <+ eliminatesType "Fingerprint")
                                         (   bracketR "MEMOIZING" memoize
                                          <+ (forcePrims ["fingerprintFingerprints", "eqWord#", "tagToEnum#"] >>> traceR "FORCING"))

optSYB :: RewriteH LCore
optSYB = (do c <- compileRememberedT
             onetdR (promoteExprR (bracketR "!!!!! USED MEMOIZED BINDING !!!!!" $ runFoldR c)))
         <+ optSimp <+ optFloat <+ optMemo

unLoopBreaker :: RewriteH CoreDef
unLoopBreaker = do
    Def i e <- idR
    guardMsg (isId i) "not an Id"
    dflags <- dynFlagsT
    return $ Def (setIdUnfolding (setIdOccInfo i NoOccInfo) (mkSimpleUnfolding dflags e)) e

loopBreaker :: RewriteH CoreDef
loopBreaker = do
    Def i e <- idR
    guardMsg (isId i) "not an Id"
    dflags <- dynFlagsT
    return $ Def (setIdOccInfo i strongLoopBreaker) e

defInfo :: TransformH CoreDef String
defInfo = do
    Def i e <- idR
    return (varToCoreExpr i) >>> (extractT infoT :: TransformH CoreExpr String)

exts ::  [External]
exts = map ((.+ Experiment) . (.+ TODO)) [
   external "un-loopbreaker" (promoteDefR unLoopBreaker :: RewriteH LCore)
       [ "Unset loopbreaker status on a binding and provide unfolding info." ]
 , external "loopbreaker" (promoteDefR loopBreaker :: RewriteH LCore)
       [ "Set loopbreaker status on a binding." ]
 , external "def-info" (promoteDefT defInfo :: TransformH LCore String)
       [ "See 'info' for binder in definition. " ]
 , external "let-subst-trivial" (promoteExprR letSubstTrivialR :: RewriteH LCore)
       [ "Let substitution (but only if e1 is a variable or literal)"
       , "let x = e1 in e2 ==> e2[e1/x]"
       , "x must not be free in e1." ] .+ Deep
 , external "letrec-subst-trivial" (promoteExprR letrecSubstTrivialR :: RewriteH LCore)
       [ "Let substitution (but only if e1 is a variable or literal)"
       , "let x = e1 in e2 ==> e2[e1/x]"
       , "x must not be free in e1." ] .+ Deep
 , external "let-subst-type" (promoteExprR . letSubstType :: String -> RewriteH LCore)
       [ "Let substitution (but only if the type of e1 contains the given type)"
       , "(\"let-subst-type '*\" is especially useful to eliminate bindings for types)"
       , "(let x = e1 in e2) ==> (e2[e1/x])"
       , "x must not be free in e1." ] .+ Deep
 , external "eval-eqWord" (promoteExprR eqWordElim :: RewriteH LCore)
        ["eqWord# e1 e2 ==> True if e1 and e2 are literals and equal"
        ,"eqWord# e1 e2 ==> False if e1 and e2 are literals and not equal"] .+ Shallow .+ Eval
 , external "eval-tagToEnum" (promoteExprR tagToEnumElim :: RewriteH LCore)
        ["tagToEnum# Bool 0 ==> True"] .+ Shallow .+ Eval
 , external "eval-fingerprintFingerprints" (promoteExprR evalFingerprintFingerprints :: RewriteH LCore)
        ["replaces 'fingerprintFingerprints [f1,f2,...]' with its value."
        ,"Requires that f1,f2,... are literals."] .+ Shallow .+ Eval
 , external "eliminates-type" (promoteExprT . eliminatesType :: String -> TransformH LCore ())
        ["determines whether evaluating the term "] .+ Shallow
 , external "smart-td" (smarttdR)
        [ "apply a rewrite twice, in a top-down and bottom-up way, using one single tree traversal",
        "succeeding if any succeed"]
 , external "force" (promoteExprR force :: RewriteH LCore)
        ["force"].+ Eval
 , external "force" (promoteExprR . forcePrims :: [String] -> RewriteH LCore)
        ["force [Name]"].+ Eval
 , external "memoize" (promoteExprR memoize :: RewriteH LCore)
        ["TODO"] .+ Eval .+ Introduce
 , external "opt-syb"   optSYB   ["TODO"] .+ Eval .+ Introduce
 , external "opt-syb-lint"   (optSYB >>> (promoteExprT lintExprT >> idR))   ["TODO"] .+ Eval .+ Introduce
 , external "opt-simp"  optSimp  ["TODO"] .+ Eval .+ Introduce
 , external "opt-float" optFloat ["TODO"] .+ Eval .+ Introduce
 , external "opt-memo"  optMemo  ["TODO"] .+ Eval .+ Introduce
 , external "memo-float-app" (promoteExprR memoFloatApp :: RewriteH LCore)
             [ "(let v = ev in e) x ==> let v = ev in e x" ]                    .+ Commute .+ Shallow
 , external "memo-float-arg" (promoteExprR memoFloatArg :: RewriteH LCore)
             [ "f (let v = ev in e) ==> let v = ev in f e" ]                    .+ Commute .+ Shallow
 , external "memo-float-lam" (promoteExprR memoFloatLam :: RewriteH LCore)
             [ "(\\ v1 -> let v2 = e1 in e2)  ==>  let v2 = e1 in (\\ v1 -> e2), if v1 is not free in e2.",
               "If v1 = v2 then v1 will be alpha-renamed."
             ]                                                                  .+ Commute .+ Shallow
 , external "memo-float-memo-let" (promoteExprR memoFloatMemoLet :: RewriteH LCore)
             [ "let v = (let w = ew in ev) in e ==> let w = ew in let v = ev in e" ] .+ Commute .+ Shallow
 , external "memo-float-memo-bind" (promoteExprR memoFloatMemoBind :: RewriteH LCore)
             [ "let v = (let w = ew in ev) in e ==> let w = ew in let v = ev in e" ] .+ Commute .+ Shallow
 , external "memo-float-let" (promoteExprR memoFloatLet :: RewriteH LCore)
             [ "let v = (let w = ew in ev) in e ==> let w = ew in let v = ev in e" ] .+ Commute .+ Shallow
 , external "memo-float-bind" (promoteExprR memoFloatBind :: RewriteH LCore)
             [ "let v = (let w = ew in ev) in e ==> let w = ew in let v = ev in e" ] .+ Commute .+ Shallow
 , external "memo-float-rec-bind" (promoteExprR memoFloatRecBind :: RewriteH LCore)
             [ "let v = (let w = ew in ev) in e ==> let w = ew in let v = ev in e" ] .+ Commute .+ Shallow
 , external "memo-float-case" (promoteExprR memoFloatCase :: RewriteH LCore)
             [ "case (let v = ev in e) of ... ==> let v = ev in case e of ..." ]  .+ Commute .+ Shallow .+ Eval
 , external "memo-float-cast" (promoteExprR memoFloatCast :: RewriteH LCore)
             [ "case (let v = ev in e) of ... ==> let v = ev in case e of ..." ]  .+ Commute .+ Shallow .+ Eval
 , external "memo-float-alt" (promoteExprR memoFloatAlt :: RewriteH LCore)
             [ "case (let v = ev in e) of ... ==> let v = ev in case e of ..." ]  .+ Commute .+ Shallow .+ Eval
 , external "memo-elim" (promoteExprR memoElimR :: RewriteH LCore)
             [ "eliminate a non-self-recursive memoization binding" ]
 ]

letSubstTrivialR :: RewriteH CoreExpr
letSubstTrivialR = prefixFailMsg "Let substition failed: " $ do
  contextfreeT $ \ expr -> case expr of
    Let (NonRec b be@(Var _)) e -> return $ substCoreExpr b be e
    Let (NonRec b be@(Lit _)) e -> return $ substCoreExpr b be e
    _ -> fail $ "expression is not a trivial, non-recursive Let."

letrecSubstTrivialR :: RewriteH CoreExpr
letrecSubstTrivialR = prefixFailMsg "Letrec substition failed: " $ do
  contextfreeT $ \ expr -> case expr of
    Let (Rec bs) e
      | Just (b, be, bs', e') <- findTrivial e [] bs -> do
        let bs'' = map (substCoreExpr b be . snd) bs'
            e'' = substCoreExpr b be e'
        return (Let (Rec (zip (map fst bs') bs'')) e'')
    _ -> fail $ "expression is not a trivial, recursive Let."

findTrivial _ _ [] = Nothing
findTrivial e bs' ((b,be) : bs) =
  case be of
    Var _ -> Just (b, be, bs' ++ bs, e)
    Lit _ -> Just (b, be, bs' ++ bs, e)
    _ -> findTrivial e (bs'++[(b,be)]) bs

letSubstType :: String -> RewriteH CoreExpr
letSubstType ty = prefixFailMsg ("letSubstType '" ++ ty ++ " failed: ") $
                  {-withPatFailMsg (wrongExprForm "Let (NonRec lhs rhs) body") $-}
   do Let (NonRec lhs rhs) body <- idR
      let t = exprKindOrType rhs
      dynFlags <- constT getDynFlags
      guardMsg (ty `inType` t) $ " not found in " ++ showPpr dynFlags t ++ "."
      letSubstR

inType :: String -> Type -> Bool
inType name ty = go ty where
 go (TyVarTy _) = False
 go (AppTy t1 t2) = go t1 || go t2
 go (TyConApp ctor args) = name `cmpString2Name` tyConName ctor || any (go) args
 go (FunTy t1 t2) = go t1 || go t2
 go (ForAllTy _ t) = go t


eqWordElim :: RewriteH CoreExpr
eqWordElim = do
    (Var v, [Lit l1, Lit l2]) <- callNameT "GHC.Prim.eqWord#"
#if __GLASGOW_HASKELL__ <= 706
    return $ mkIntLitInt $ if l1 == l2 then 1 else 0
#else
    dflags <- constT getDynFlags
    return $ mkIntLitInt dflags $ if l1 == l2 then 1 else 0
#endif

#if __GLASGOW_HASKELL__ >= 708
tagToEnumElim :: RewriteH CoreExpr
tagToEnumElim = do
  (Var v, [Type ty, Lit (MachInt i)]) <- callNameT "tagToEnum#"
  case splitTyConApp_maybe ty of
    Just (tycon, tc_args) | isEnumerationTyCon tycon -> do
      let tag = fromInteger i
          correct_tag dc = (dataConTag dc - fIRST_TAG) == tag
      (dc:rest) <- return $ filter correct_tag (maybe [] id $ tyConDataCons_maybe tycon)
      --ASSERT(null rest) return ()
      return $ mkTyApps (Var (dataConWorkId dc)) tc_args
    _ -> fail "tagToEnum# on non-enumeration type"
#endif

varInfo2 :: String -> TransformH LCore String
varInfo2 nm = translate $ \ c e ->
  case filter (cmpString2Var nm) $ Map.keys (hermitBindings c) of
         []  -> fail "cannot find name."
         [i] -> do dynFlags <- getDynFlags
                   return ("Type or Kind: " ++ (showPpr dynFlags . exprKindOrType) (Var i))
         is  -> fail $ "multiple names match: " ++ intercalate ", " (map unqualifiedName is)

varInfo :: RewriteH CoreExpr
varInfo = do
  Var v <- idR
  dynFlags <- constT getDynFlags
  trace (Outputable.showSDocDebug dynFlags (ppr v)) (return (Var v))

evalFingerprintFingerprints :: RewriteH CoreExpr
evalFingerprintFingerprints = do
  dynFlags <- constT getDynFlags
  App (Var v) args <- idR
  --trace ("looking at: "++ showPpr dynFlags v) $ return ()
  v' <- findIdT "fingerprintFingerprints"
  --trace ("binding: "++showPpr dynFlags v') $ return ()
  ctor <- findIdT "Fingerprint"
  --trace ("ctor: "++showPpr dynFlags ctor) $ return ()
  w64 <- findIdT "GHC.Word.W64#"
  guardMsg (v == v') (unqualifiedName v ++ " does not match " ++ "fingerprintFingerprints")
  let getFingerprints (App (Var nil') _) {- - | nil' == nil -} = return []
      getFingerprints (Var cons' `App` _ `App` f `App` fs) {- - | cons' == cons-} = liftM2 (:) f' (getFingerprints fs) where
         f' = case f of (Var ctor' `App` (Lit (MachWord w1)) `App` (Lit (MachWord w2)))
                            {- - | ctor' == ctor-} -> return (Fingerprint (fromIntegral w1) (fromIntegral w2))
                        _ -> fail ("Non-literal fingerprint as argument:"++gshow f)
      getFingerprints a = fail ("Non-list as argument:"++gshow a)
  fingerprints <- getFingerprints args
  let Fingerprint w1 w2 = fingerprintFingerprints fingerprints
  return (Var ctor `App` (Var w64 `App` Lit (MachWord (toInteger w1))) `App` (Var w64 `App` Lit (MachWord (toInteger w2))))
  --return (Var ctor `App` (Lit (MachWord (toInteger w1))) `App` (Lit (MachWord (toInteger w2))))

isMemoizedLet :: TransformH LCore ()
isMemoizedLet = do
  e <- idR
  case e of
    LCore (ExprCore (Let (Rec [(v, _)]) _)) -> do
      flags <- constT $ getDynFlags
      void $ constT $ lookupDefByVar flags v
    _ -> fail "not a memoized let"

inlineType :: String -> RewriteH CoreExpr
inlineType ty = prefixFailMsg ("inlineType '" ++ ty ++ " failed: ") $
                withPatFailMsg (wrongExprForm "Var v") $
   do Var v <- idR
      let t = (exprKindOrType (Var v))
      dynFlags <- constT getDynFlags
      guardMsg (ty `inType` t) $ " not found in " ++ showPpr dynFlags t ++ "."
      inlineR

-- | Apply a 'Rewrite' in a bottom-up manner, succeeding if they all succeed.
smarttdR :: RewriteH LCore -> RewriteH LCore
smarttdR r = modFailMsg ("smarttdR failed: " ++) $ go where
  go = r <+ go'
  go' = do
    LCore (ExprCore e) <- idR
    case e of
      Var _ -> fail "smarttdR Var"
      Lit _ -> fail "smarttdR Let"
      App _ _ -> pathR [App_Fun] go <+ pathR [App_Arg] go
      Lam _ _ -> pathR [Lam_Body] go
      Let (NonRec _ _) _ -> pathR [Let_Body] go <+ pathR [Let_Bind,NonRec_RHS] go
      Let (Rec bs) _ -> pathR [Let_Body] go <+ foldr (<+) (fail "smarttdR Let Rec") [ pathR [Let_Bind, Rec_Def i, Def_RHS] go
                                                                                    | (i, _) <- zip [0..] bs]
      Case _ _ _ as -> pathR [Case_Scrutinee] go <+ foldr (<+) (fail "smarttdR Case") [pathR [Case_Alt i, Alt_RHS] go | (i, _) <- zip [0..] as]
      Cast _ _ -> pathR [Cast_Expr] go
      Tick _ _ -> pathR [Tick_Expr] go
      Type _ -> fail "smarttdR Type"
      Coercion _ -> fail "smarttdR Coercion"

eliminatesType :: String -> TransformH CoreExpr ()
eliminatesType ty = do
    dynFlags <- constT getDynFlags
    prefixFailMsg ("eliminatesType: " ++ ty ++ " ") $ do
        appT successT (inTypeT ty) const
        <+ castT (inTypeT ty) successT (flip const)
        <+ caseT (inTypeT ty) successT successT (const successT) (\() _ _ _ -> ())

{-
  (ExprCore e) <- idR
  case e of
    Var _ -> fail "vars cannot eliminate types"
    Lit _ -> fail "lits cannot eliminate types"
    Lam _ _ -> fail "lams cannot eliminate types"
    App _ arg -> go arg
    Case scrut _ _ _ -> go scrut
    -- Syntactic noise that we skip over (but may have to float)
    Let _ _body -> fail "lets cannot eliminate types"
    Cast body _ -> go body
    -- Errors that we never expect to see
    Tick _ _ -> fail "ticks cannot eliminate types"
      {-^ not really an error, I just don't know what to do with it -}
    Type _ -> fail "types cannot eliminate types"
    Coercion _ -> fail "coercions cannot eliminate types"
  where go :: CoreExpr -> TransformH LCore ()
        go e = do let t = exprKindOrType e
                  dynFlags <- constT getDynFlags
                  guardMsg (ty `inType` t) $ " not found in " ++ showPpr dynFlags t ++ "."
                  return ()
-}

inTypeT :: String -> TransformH CoreExpr ()
inTypeT ty = do
    dynFlags <- constT getDynFlags
    contextfreeT $ \ e -> do
        let t = exprKindOrType e
        guardMsg (ty `inType` t) $ " not found in " ++ showPpr dynFlags t ++ "."
        return ()

force :: RewriteH CoreExpr
force = forcePrims []

forcePrims :: [String] -> RewriteH CoreExpr
forcePrims prims = forceDeep False prims

forceDeep :: Bool -> [String] -> RewriteH CoreExpr
forceDeep deep prims =
       inlineR
    <+ betaReduceR <+ letFloatAppR <+ castFloatAppR
    <+ (appT (isPrimCall' prims) successT const >> appAllR idR (forceDeep True prims))
    <+ appAllR (forceDeep deep prims) idR
    <+ whenM (return deep) (appAllR idR (forceDeep deep prims))
    <+ caseAllR (forceDeep deep prims) idR idR (const idR) <+ caseReduceR False <+ letFloatCaseR
    <+ letAllR idR (forceDeep deep prims)
    <+ castAllR (forceDeep deep prims) idR
    <+ fail "forceDeep failed"

-- TODO: there is probably a built-in for doing this wrapping already but I don't know what it is
isPrimCall' :: [String] -> TransformH CoreExpr ()
isPrimCall' prims = do
  e <- idR
  if isPrimCall prims e then return () else fail "not a prim call"

isPrimCall :: [String] -> CoreExpr -> Bool
isPrimCall prims (Var v) = any (flip cmpString2Var v) prims
isPrimCall prims (App f _) = isPrimCall prims f
isPrimCall _ _ = False

memoize :: RewriteH CoreExpr
memoize = prefixFailMsg "memoize failed: " $ do
    e <- idR
    (Var v, args) <- callT
    --e' <- extractR (pathR (replicate (length args) 0) (promoteR {-force-} inline) :: RewriteH LCore)
    e' <- force
    v' <- constT $ newIdH ("memo_"++getOccString v) (exprType e)
    dflags <- dynFlagsT
    let v'' = setIdUnfolding v' (mkSimpleUnfolding dflags e)
    extractT (rememberR (fromString (showPpr dflags v''))) <<< return (Def v'' e)
    --cleanupUnfold
    --e' <- idR
    --e' <- translate $ \env _ -> apply (extractR inline) env (Var v)
    return (Let (Rec [(v'', e')]) (Var v''))

-------------------------------------------------------------------------------------------

-- | Attempt to eliminate any non-self-recursive memoized bindings.
memoElimR :: RewriteH CoreExpr
memoElimR = prefixFailMsg "Memoized binding elimination failed: " $ do
    Let bnds body <- idR
    dflags <- getDynFlags
    case bnds of
        NonRec v rhs -> do
            setFailMsg "not a memoization binding." $ constT $ lookupDefByVar dflags v
            letNonRecSubstR
        Rec defs -> do
            guardMsg (notNull defs) "empty recursive defs."
            let findInlineTarget [] = fail "no non-recursive memoization bindings in group."
                findInlineTarget ((v,rhs):rest) = do
                    f1 <- testM $ lookupDefByVar dflags v
                    let f2 = not (v `elemVarSet` (freeVarsExpr rhs))
                    if f1 && f2 then return (v,rhs) else findInlineTarget rest
            (v,rhs) <- constT $ findInlineTarget $ reverse defs
            let substAllRhss [] = []
                substAllRhss ((b,e):rest) | b == v    = substAllRhss rest
                                          | otherwise = (b,substCoreExpr v rhs e) : substAllRhss rest
            return $ Let (Rec $ substAllRhss defs) (substCoreExpr v rhs body)

memoFloatApp :: RewriteH CoreExpr
memoFloatApp = prefixFailMsg "Let floating from App function failed: " $
  do flags <- constT getDynFlags
     appT letVarsT idR $ \x y -> mapM (lookupDefByVar flags) x
     vs <- appT letVarsT (arr $ varSetElems . freeVarsExpr) intersect
     let letAction = if null vs then idR else alphaLetR
     appT letAction idR $ \ (Let bnds e) x -> Let bnds $ App e x

memoFloatArg :: RewriteH CoreExpr
memoFloatArg = prefixFailMsg "Let floating from App argument failed: " $
  do flags <- constT getDynFlags
     appT idR letVarsT $ \x y -> mapM (lookupDefByVar flags) y
     vs <- appT (arr $ varSetElems . freeVarsExpr) letVarsT intersect
     let letAction = if null vs then idR else alphaLetR
     appT idR letAction $ \ f (Let bnds e) -> Let bnds $ App f e

memoFloatMemoLet :: RewriteH CoreExpr
memoFloatMemoLet = prefixFailMsg "Let floating from Let failed: " $ do
  (Let (Rec binds1) (Let (Rec binds2) body)) <- idR
  flags <- constT getDynFlags
  constT $ mapM (lookupDefByVar flags . fst) binds1
  constT $ mapM (lookupDefByVar flags . fst) binds2
  return (Let (Rec (binds1 ++ binds2)) body)

memoFloatLet :: RewriteH CoreExpr
memoFloatLet = prefixFailMsg "Let floating from Let failed: " $ do
  (Let binds1 (Let (Rec binds2) body)) <- idR
  vars <- letVarsT
  flags <- constT getDynFlags
  constT $ mapM (lookupDefByVar flags . fst) binds2
  let (unfloatable, floatable) = filterBinds' vars binds2
  if null floatable
  then fail "cannot float"
  else return (Let (Rec floatable) (Let binds1
                 (if null unfloatable
                  then body
                  else Let (Rec unfloatable) body)))

memoFloatMemoBind :: RewriteH CoreExpr
memoFloatMemoBind = prefixFailMsg "Let floating from Let failed: " $ do
  (Let (Rec binds1) body) <- idR
  flags <- constT getDynFlags
  constT $ mapM (lookupDefByVar flags . fst) binds1
  let f (x, e) = (do (Let (Rec binds2) body) <- return e
                     constT $ mapM (lookupDefByVar flags . fst) binds2
                     return ((x, body) : binds2)) <+
                 (return [(x, e)])
  binds1' <- mapM f binds1
  if length (concat binds1') == length binds1
  then fail "cannot float"
  else return (Let (Rec (concat binds1')) body)

memoFloatRecBind :: RewriteH CoreExpr
memoFloatRecBind = prefixFailMsg "Let floating from Let failed: " $ do
  (Let (Rec binds1) body) <- idR
  flags <- constT getDynFlags
  constT $ mapM (lookupDefByVar flags . fst) binds1
  let f (x, e) = (do (Let (Rec binds2) body) <- return e
                     constT $ mapM (lookupDefByVar flags . fst) binds2
                     let (unfl, fl) = filterBinds' (map fst binds1) binds2
                     return (fl, (x, if null unfl then body else Let (Rec unfl) body))) <+
                 (return ([], (x, e)))
  binds1' <- mapM f binds1
  (outer, inner) <- return (unzip binds1')
  if null (concat outer)
  then fail "cannot float"
  else return (Let (Rec (concat outer)) (Let (Rec inner) body))

memoFloatBind :: RewriteH CoreExpr
memoFloatBind = prefixFailMsg "Let floating from Let failed: " $ do
  (Let (NonRec lhs (Let (Rec binds) rhs)) body) <- idR
  flags <- constT getDynFlags
  constT $ mapM (lookupDefByVar flags . fst) binds
  let (unfloatable, floatable) = filterBinds' [lhs] binds
  if null floatable
  then fail "cannot float"
  else return (Let (Rec floatable) (Let (NonRec lhs (
         if null unfloatable then rhs else Let (Rec unfloatable) rhs)) body))

filterBinds' vars binds = filterBinds vars [] (map (\x -> (varSetElems $ freeIdsExpr (snd x), x)) binds)

filterBinds :: [Id] -> [(Id, a)]
            -> [([Id]{-free vars-}, (Id{-lhs-}, a{-rhs-}))]
            -> ([(Id, a)], [(Id, a)])
filterBinds vars unfloatables floatables =
  case partition isUnfloatable floatables of
    ([], _) -> (unfloatables, map snd floatables)
    (newUnfloatable, newFloatables) ->
      filterBinds
        (map (fst . snd) newUnfloatable ++ vars)
        (map snd newUnfloatable ++ unfloatables)
        newFloatables
  where isUnfloatable (vars', _) = not (null (intersect vars vars'))

lookupDefByVar :: DynFlags -> Var -> HermitM Lemma
lookupDefByVar flags v = do
    let nm = fromString $ showPpr flags v
    ls <- getLemmas
    maybe (fail $ "No lemma named: " ++ show nm) return $ Map.lookup (prefixRemembered nm) ls

memoFloatLam :: RewriteH CoreExpr
memoFloatLam = prefixFailMsg "Let floating from Lam failed: " $
              withPatFailMsg (wrongExprForm "Lam v1 (Let (NonRec v2 e1) e2)") $
  do Lam v1 (Let (Rec binds) e2) <- idR
     flags <- constT getDynFlags
     constT $ mapM (lookupDefByVar flags . fst) binds
     if v1 `elem` (map fst binds)
     then alphaLamR Nothing >>> memoFloatLam
     else let (unfloatable, floatable) = filterBinds' [v1] binds
       in if null floatable
          then fail "no floatable let binds"
--     mapM (\(v2, e1) -> guardMsg (v1 `notElem` coreExprFreeVars e1) $ unqualifiedName v1 ++ " occurs in the definition of " ++ unqualifiedName v2 ++ ".") binds
          else return (Let (Rec floatable) (Lam v1
                        (if null unfloatable
                        then e2
			else Let (Rec unfloatable) e2)))

memoFloatCase :: RewriteH CoreExpr
memoFloatCase = prefixFailMsg "Let floating from Case failed: " $
  do flags <- constT getDynFlags
     caseT letVarsT idR idR (const idR) $ \x _ _ _ -> mapM (lookupDefByVar flags) x
     captures <- caseT letVarsT idR idR
                       (\ _ -> arr $ varSetElems . freeVarsAlt)
                       (\ vs wild _ fs -> vs `intersect` concatMap (wild:) fs)
     caseT (if null captures then idR else alphaLetVarsR captures)
           idR idR
           (const idR)
           (\ (Let bnds e) wild ty alts -> Let bnds (Case e wild ty alts))

memoFloatCast :: RewriteH CoreExpr
memoFloatCast = prefixFailMsg "Let floating from Cast failed: " $
  do flags <- constT getDynFlags
     castT letVarsT idR $ \x co -> mapM (lookupDefByVar flags) x
     castT idR idR (\ (Let bnds e) co -> Let bnds (Cast e co))

-- | @case (let bnds in e) of wild alts ==> let bnds in (case e of wild alts)@
--   Fails if any variables bound in @bnds@ occurs in @alts@.
memoFloatAlt :: RewriteH CoreExpr
memoFloatAlt = prefixFailMsg "Let floating from Case failed: " $ do
  -- rename if any conflict
  -- process each
  e' <- idR
  flags <- constT getDynFlags
  e@(Case scr wild ty alts) <- idR
  let getLet pre post alt = do
        (con, args, Let (Rec binds) body) <- return alt
        -- TODO: intersect "args" with "free args of binds".  alpha?
        constT $ mapM (lookupDefByVar flags . fst) binds
        let (unfloatable, floatable) = filterBinds' args binds
        if null floatable
        then fail "no floatable let binds"
        else return (Let (Rec floatable) (Case scr wild ty (pre ++
           (con, args, if null unfloatable then body else Let (Rec unfloatable) body) : post)))
      getLets pre [] = fail "no memo lets found"
      getLets pre (alt : post) =
        (getLet pre post alt) <+ getLets (pre ++ [alt]) post
  getLets [] alts

