{-# LANGUAGE DoAndIfThenElse #-} -- why do we need this?
module HERMIT.Optimization.SYB where

import qualified Outputable (showSDocDebug)

import Control.Arrow
import Control.Monad
import Data.Generics (gshow)
import Data.List (intercalate, intersect, partition)
import GHC.Fingerprint (Fingerprint(..), fingerprintFingerprints)

import qualified Data.Map as Map

import HERMIT.Context
import HERMIT.Core
import HERMIT.Dictionary
import HERMIT.Monad
import HERMIT.Kure
import HERMIT.External
import HERMIT.GHC
import HERMIT.Optimize

import qualified Language.Haskell.TH as TH

plugin :: Plugin
plugin = optimize $ \ opts -> phase 0 $ do
    let (opts', targets) = partition (`elem` ["interactive", "interactive-only"]) opts
    if "interactive-only" `elem` opts'
    then return ()
    else forM_ targets $ \ t -> do
            liftIO $ putStrLn $ "optimizing: " ++ t
            at (rhsOfT $ cmpTHName2Var $ TH.mkName t) $ do
                run $ optSYB >>> tryR (innermostR $ promoteExprR letrecSubstTrivialR) >>> tryR simplifyR

    -- pass either interactive or interactive-only flags to dump into a shell at the end
    if null opts'
    then return ()
    else interactive exts []

optSYB :: RewriteH Core
optSYB = do
    repeatR (onetdR (promoteExprR stashFoldAnyR >>> traceR "!!!!! USED MEMOIZED BINDING !!!!!!")
                          <+ traceR "MID" >>> anytdR (repeatR (promoteExprR (
                                                                   rule "append"
                                                                <+ rule "[]++"
                                                                <+ castElimReflR
                                                                <+ castElimSymPlusR
                                                                <+ letElimR
                                                                <+ letSubstType (TH.mkName "*")
                                                                <+ letSubstType (TH.mkName "BOX")
                                                                <+ evalFingerprintFingerprints
                                                                <+ eqWordElim
                                                                <+ letSubstTrivialR
                                                                <+ caseReduceR)
                                                               >>> traceR "SIMPLIFYING"))
                                              <+ anybuR (promoteExprR ((
                                                              memoFloatMemoLet
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
                                              <+ smarttdR ((>>) (eliminatesType (TH.mkName "Data")
                                                                  <+ eliminatesType (TH.mkName "Typeable")
                                                                  <+ eliminatesType (TH.mkName "Typeable1")
                                                                  <+ eliminatesType (TH.mkName "TypeRep")
                                                                  <+ eliminatesType (TH.mkName "TyCon")
                                                                  <+ eliminatesType (TH.mkName "ID")
                                                                  <+ eliminatesType (TH.mkName "Qr")
                                                                  <+ eliminatesType (TH.mkName "Fingerprint"))
                                                                 ((promoteExprR memoize >>> traceR "MEMOIZING")
                                                                  <+ (promoteExprR (forcePrims [TH.mkName "fingerprintFingerprints", TH.mkName "eqWord#"])
                                                                      >>> traceR "FORCING"))))

exts ::  [External]
exts = map ((.+ Experiment) . (.+ TODO)) [
   external "let-subst-trivial" (promoteExprR letSubstTrivialR :: RewriteH Core)
       [ "Let substitution (but only if e1 is a variable or literal)"
       , "let x = e1 in e2 ==> e2[e1/x]"
       , "x must not be free in e1." ] .+ Deep
 , external "letrec-subst-trivial" (promoteExprR letrecSubstTrivialR :: RewriteH Core)
       [ "Let substitution (but only if e1 is a variable or literal)"
       , "let x = e1 in e2 ==> e2[e1/x]"
       , "x must not be free in e1." ] .+ Deep
 , external "let-subst-type" (promoteExprR . letSubstType :: TH.Name -> RewriteH Core)
       [ "Let substitution (but only if the type of e1 contains the given type)"
       , "(\"let-subst-type '*\" is especially useful to eliminate bindings for types)"
       , "(let x = e1 in e2) ==> (e2[e1/x])"
       , "x must not be free in e1." ] .+ Deep
 , external "eval-eqWord" (promoteExprR eqWordElim :: RewriteH Core)
        ["eqWord# e1 e2 ==> True if e1 and e2 are literals and equal"
        ,"eqWord# e1 e2 ==> False if e1 and e2 are literals and not equal"] .+ Shallow .+ Eval
 , external "eval-fingerprintFingerprints" (promoteExprR evalFingerprintFingerprints :: RewriteH Core)
        ["replaces 'fingerprintFingerprints [f1,f2,...]' with its value."
        ,"Requires that f1,f2,... are literals."] .+ Shallow .+ Eval
 , external "eliminates-type" (eliminatesType)
        ["determines whether evaluating the term "] .+ Shallow
 , external "smart-td" (smarttdR)
        [ "apply a rewrite twice, in a top-down and bottom-up way, using one single tree traversal",
        "succeeding if any succeed"]
 , external "force" (promoteExprR force :: RewriteH Core)
        ["cast-elim removes casts"].+ Eval
 , external "force" (promoteExprR . forcePrims :: [TH.Name] -> RewriteH Core)
        ["cast-elim removes casts"].+ Eval
 , external "memoize" (promoteExprR memoize :: RewriteH Core)
        ["TODO"] .+ Eval .+ Introduce
 , external "opt-syb" (optSYB)
        ["TODO"] .+ Eval .+ Introduce
         , external "memo-float-app" (promoteExprR memoFloatApp :: RewriteH Core)
                     [ "(let v = ev in e) x ==> let v = ev in e x" ]                    .+ Commute .+ Shallow
         , external "memo-float-arg" (promoteExprR memoFloatArg :: RewriteH Core)
                     [ "f (let v = ev in e) ==> let v = ev in f e" ]                    .+ Commute .+ Shallow
         , external "memo-float-lam" (promoteExprR memoFloatLam :: RewriteH Core)
                     [ "(\\ v1 -> let v2 = e1 in e2)  ==>  let v2 = e1 in (\\ v1 -> e2), if v1 is not free in e2.",
                       "If v1 = v2 then v1 will be alpha-renamed."
                     ]                                                                  .+ Commute .+ Shallow
         , external "memo-float-memo-let" (promoteExprR memoFloatMemoLet :: RewriteH Core)
                     [ "let v = (let w = ew in ev) in e ==> let w = ew in let v = ev in e" ] .+ Commute .+ Shallow
         , external "memo-float-memo-bind" (promoteExprR memoFloatMemoBind :: RewriteH Core)
                     [ "let v = (let w = ew in ev) in e ==> let w = ew in let v = ev in e" ] .+ Commute .+ Shallow
         , external "memo-float-let" (promoteExprR memoFloatLet :: RewriteH Core)
                     [ "let v = (let w = ew in ev) in e ==> let w = ew in let v = ev in e" ] .+ Commute .+ Shallow
         , external "memo-float-bind" (promoteExprR memoFloatBind :: RewriteH Core)
                     [ "let v = (let w = ew in ev) in e ==> let w = ew in let v = ev in e" ] .+ Commute .+ Shallow
         , external "memo-float-rec-bind" (promoteExprR memoFloatRecBind :: RewriteH Core)
                     [ "let v = (let w = ew in ev) in e ==> let w = ew in let v = ev in e" ] .+ Commute .+ Shallow
         , external "memo-float-case" (promoteExprR memoFloatCase :: RewriteH Core)
                     [ "case (let v = ev in e) of ... ==> let v = ev in case e of ..." ]  .+ Commute .+ Shallow .+ Eval
         , external "memo-float-cast" (promoteExprR memoFloatCast :: RewriteH Core)
                     [ "case (let v = ev in e) of ... ==> let v = ev in case e of ..." ]  .+ Commute .+ Shallow .+ Eval
         , external "memo-float-alt" (promoteExprR memoFloatAlt :: RewriteH Core)
                     [ "case (let v = ev in e) of ... ==> let v = ev in case e of ..." ]  .+ Commute .+ Shallow .+ Eval
 ]

letSubstTrivialR :: RewriteH CoreExpr
letSubstTrivialR = prefixFailMsg "Let substition failed: " $ do
  rewrite $ \ c expr -> case expr of
    Let (NonRec b be@(Var _)) e -> apply (substExprR b be) c e
    Let (NonRec b be@(Lit _)) e -> apply (substExprR b be) c e
    _ -> fail $ "expression is not a trivial, non-recursive Let."

letrecSubstTrivialR :: RewriteH CoreExpr
letrecSubstTrivialR = prefixFailMsg "Letrec substition failed: " $ do
  rewrite $ \ c expr -> case expr of
    Let (Rec bs) e
      | Just (b, be, bs', e') <- findTrivial e [] bs -> do
        bs'' <- mapM (apply (substExprR b be) c . snd) bs'
        e'' <- apply (substExprR b be) c e'
        return (Let (Rec (zip (map fst bs') bs'')) e'')
    _ -> fail $ "expression is not a trivial, recursive Let."

findTrivial _ _ [] = Nothing
findTrivial e bs' ((b,be) : bs) =
  case be of
    Var _ -> Just (b, be, bs' ++ bs, e)
    Lit _ -> Just (b, be, bs' ++ bs, e)
    _ -> findTrivial e (bs'++[(b,be)]) bs

letSubstType :: TH.Name -> RewriteH CoreExpr
letSubstType ty = prefixFailMsg ("letSubstType '" ++ TH.nameBase ty ++ " failed: ") $
                  {-withPatFailMsg (wrongExprForm "Let (NonRec lhs rhs) body") $-}
   do Let (NonRec lhs rhs) body <- idR
      let t = exprKindOrType rhs
      dynFlags <- constT getDynFlags
      guardMsg (ty `inType` t) $ " not found in " ++ showPpr dynFlags t ++ "."
      letSubstR

inType :: TH.Name -> Type -> Bool
inType name ty = go ty where
 go (TyVarTy _) = False
 go (AppTy t1 t2) = go t1 || go t2
 go (TyConApp ctor args) = name `cmpTHName2Name` tyConName ctor || any (go) args
 go (FunTy t1 t2) = go t1 || go t2
 go (ForAllTy _ t) = go t


eqWordElim :: RewriteH CoreExpr
eqWordElim = do
  (App (App (Var v) (Lit l1)) (Lit l2)) <- idR
  eqWordId <- findIdT (TH.mkName "GHC.Prim.eqWord#")
  guardMsg (v == eqWordId) (var2String v ++ " does not match " ++ "GHC.Prim.eqWord#")
  liftM Var (findIdT (TH.mkName (if l1 == l2 then "GHC.Types.True" else "GHC.Types.False")))

varInfo2 :: TH.Name -> TranslateH Core String
varInfo2 nm = translate $ \ c e ->
  case filter (cmpTHName2Var nm) $ Map.keys (hermitBindings c) of
         []  -> fail "cannot find name."
         [i] -> do dynFlags <- getDynFlags
                   return ("Type or Kind: " ++ (showPpr dynFlags . exprKindOrType) (Var i))
         is  -> fail $ "multiple names match: " ++ intercalate ", " (map var2String is)

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
  v' <- findIdT (TH.mkName "fingerprintFingerprints")
  --trace ("binding: "++showPpr dynFlags v') $ return ()
  ctor <- findIdT (TH.mkName "Fingerprint")
  --trace ("ctor: "++showPpr dynFlags ctor) $ return ()
  w64 <- findIdT (TH.mkName "GHC.Word.W64#")
  --nil <- findIdT (TH.mkName "GHC.Types.[]")
  --cons <- findIdT (TH.mkName "GHC.Types.(:)")
  guardMsg (v == v') (var2String v ++ " does not match " ++ "fingerprintFingerprints")
  let getFingerprints (App (Var nil') _) {- | nil' == nil-} = return []
      getFingerprints (Var cons' `App` _ `App` f `App` fs) {-| cons' == cons-} = liftM2 (:) f' (getFingerprints fs) where
         f' = case f of (Var ctor' `App` (Lit (MachWord w1)) `App` (Lit (MachWord w2)))
                            {-| ctor' == ctor-} -> return (Fingerprint (fromIntegral w1) (fromIntegral w2))
                        _ -> fail ("Non-literal fingerprint as argument:"++gshow f)
      getFingerprints a = fail ("Non-list as argument:"++gshow a)
  fingerprints <- getFingerprints args
  let Fingerprint w1 w2 = fingerprintFingerprints fingerprints
  return (Var ctor `App` (Var w64 `App` Lit (MachWord (toInteger w1))) `App` (Var w64 `App` Lit (MachWord (toInteger w2))))
  --return (Var ctor `App` (Lit (MachWord (toInteger w1))) `App` (Lit (MachWord (toInteger w2))))

isMemoizedLet :: TranslateH Core ()
isMemoizedLet = do
  e <- idR
  case e of
    ExprCore (Let (Rec [(v, _)]) _) -> do
      flags <- constT $ getDynFlags
      constT $ lookupDef (showPpr flags v)
      return ()
    _ -> fail "not a memoized let"

inlineType :: TH.Name -> RewriteH CoreExpr
inlineType ty = prefixFailMsg ("inlineType '" ++ TH.nameBase ty ++ " failed: ") $
                withPatFailMsg (wrongExprForm "Var v") $
   do Var v <- idR
      let t = (exprKindOrType (Var v))
      dynFlags <- constT getDynFlags
      guardMsg (ty `inType` t) $ " not found in " ++ showPpr dynFlags t ++ "."
      inlineR

-- | Apply a 'Rewrite' in a bottom-up manner, succeeding if they all succeed.
smarttdR :: RewriteH Core -> RewriteH Core
smarttdR r = modFailMsg ("smarttdR failed: " ++) $ go where
  go = r <+ go'
  go' = do
    ExprCore e <- idR
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

eliminatesType :: TH.Name -> TranslateH Core ()
eliminatesType ty = do
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
  where go :: CoreExpr -> TranslateH Core ()
        go e = do let t = exprKindOrType e
                  dynFlags <- constT getDynFlags
                  guardMsg (ty `inType` t) $ " not found in " ++ showPpr dynFlags t ++ "."
                  return ()

inTypeT :: TH.Name -> TranslateH CoreExpr ()
inTypeT ty = contextfreeT $ \ e -> do
    guardMsg (ty `inType` (exprKindOrType e)) " not found."
    return ()

force :: RewriteH CoreExpr
force = forcePrims []

forcePrims :: [TH.Name] -> RewriteH CoreExpr
forcePrims prims = extractR $ forceDeep False prims

forceDeep :: Bool -> [TH.Name] -> RewriteH Core
forceDeep deep prims = do
  ExprCore e <- idR
  case e of
    -- Core reductions
    Var _ -> promoteExprR inlineR
    Lit _ -> fail "literal is already normal"
    Lam _ _ -> fail "lambda is already normal"
    App _ _ -> promoteExprR (betaReduceR <+ letFloatAppR <+ castFloatAppR) <+
               (pathT [App_Fun] (promoteExprT (isPrimCall' prims)) 
                >> pathR [App_Arg] (forceDeep True prims)) <+
               pathT [App_Fun] (forceDeep deep prims) <+
               (if deep then pathR [App_Arg] (forceDeep deep prims) else fail "TODO")
    Case _scrut _ _ _ -> pathR [Case_Scrutinee] (forceDeep deep prims) <+ promoteExprR (caseReduceR <+ letFloatCaseR)
    -- Syntactic noise that we skip over (but may have to float)
    Let _ _body -> pathR [Let_Body] $ forceDeep deep prims
    Cast _body _ -> pathR [Cast_Expr] $ forceDeep deep prims
    -- Errors that we never expect to see
    Tick _ _ -> fail "unsupported tick in force"
      {-^ not really an error, I just don't know what to do with it -}
    Type _ -> fail "unexpected type in force"
    Coercion _ -> fail "unexpected coercion in force"
-- TODO: better error message on App, etc

-- TODO: there is probably a built-in for doing this wrapping already but I don't know what it is
isPrimCall' :: [TH.Name] -> TranslateH CoreExpr ()
isPrimCall' prims = do
  e <- idR
  if isPrimCall prims e then return () else fail "not a prim call"

isPrimCall :: [TH.Name] -> CoreExpr -> Bool
isPrimCall prims (Var v) = any (flip cmpTHName2Var v) prims
isPrimCall prims (App f _) = isPrimCall prims f
isPrimCall _ _ = False

memoize :: RewriteH CoreExpr
memoize = do
  e <- idR
  case collectArgs e of
    (Var v,args) -> do
      --e' <- extractR (pathR (replicate (length args) 0) (promoteR {-force-} inline) :: RewriteH Core)
      e' <- force
      v' <- constT $ newIdH ("memo_"++getOccString v) (exprType e)
      flags <- constT $ getDynFlags
      constT $ saveDef (showPpr flags v') (Def v' e)
      --cleanupUnfold
      --e' <- idR
      --e' <- translate $ \env _ -> apply (extractR inline) env (Var v)
      return (Let (Rec [(v', e')]) (Var v'))
    _ -> fail "memoize failed"

-------------------------------------------------------------------------------------------

memoFloatApp :: RewriteH CoreExpr
memoFloatApp = prefixFailMsg "Let floating from App function failed: " $
  do flags <- constT getDynFlags
     appT letVarsT idR $ \x y -> mapM (lookupDef . showPpr flags) x
     vs <- appT letVarsT (arr $ varSetElems . freeVarsExpr) intersect
     let letAction = if null vs then idR else alphaLetR
     appT letAction idR $ \ (Let bnds e) x -> Let bnds $ App e x

memoFloatArg :: RewriteH CoreExpr
memoFloatArg = prefixFailMsg "Let floating from App argument failed: " $
  do flags <- constT getDynFlags
     appT idR letVarsT $ \x y -> mapM (lookupDef . showPpr flags) y
     vs <- appT (arr $ varSetElems . freeVarsExpr) letVarsT intersect
     let letAction = if null vs then idR else alphaLetR
     appT idR letAction $ \ f (Let bnds e) -> Let bnds $ App f e

memoFloatMemoLet :: RewriteH CoreExpr
memoFloatMemoLet = prefixFailMsg "Let floating from Let failed: " $ do
  (Let (Rec binds1) (Let (Rec binds2) body)) <- idR
  flags <- constT getDynFlags
  constT $ mapM (lookupDef . showPpr flags . fst) binds1
  constT $ mapM (lookupDef . showPpr flags . fst) binds2
  return (Let (Rec (binds1 ++ binds2)) body)

memoFloatLet :: RewriteH CoreExpr
memoFloatLet = prefixFailMsg "Let floating from Let failed: " $ do
  (Let binds1 (Let (Rec binds2) body)) <- idR
  vars <- letVarsT
  flags <- constT getDynFlags
  constT $ mapM (lookupDef . showPpr flags . fst) binds2
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
  constT $ mapM (lookupDef . showPpr flags . fst) binds1
  let f (x, e) = (do (Let (Rec binds2) body) <- return e
                     constT $ mapM (lookupDef . showPpr flags . fst) binds2
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
  constT $ mapM (lookupDef . showPpr flags . fst) binds1
  let f (x, e) = (do (Let (Rec binds2) body) <- return e
                     constT $ mapM (lookupDef . showPpr flags . fst) binds2
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
  constT $ mapM (lookupDef . showPpr flags . fst) binds
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


memoFloatLam :: RewriteH CoreExpr
memoFloatLam = prefixFailMsg "Let floating from Lam failed: " $
              withPatFailMsg (wrongExprForm "Lam v1 (Let (NonRec v2 e1) e2)") $
  do Lam v1 (Let (Rec binds) e2) <- idR
     flags <- constT getDynFlags
     constT $ mapM (lookupDef . showPpr flags . fst) binds
     if v1 `elem` (map fst binds)
     then alphaLamR Nothing >>> memoFloatLam
     else let (unfloatable, floatable) = filterBinds' [v1] binds
       in if null floatable
          then fail "no floatable let binds"
--     mapM (\(v2, e1) -> guardMsg (v1 `notElem` coreExprFreeVars e1) $ var2String v1 ++ " occurs in the definition of " ++ var2String v2 ++ ".") binds
          else return (Let (Rec floatable) (Lam v1
                        (if null unfloatable
                        then e2
			else Let (Rec unfloatable) e2)))

memoFloatCase :: RewriteH CoreExpr
memoFloatCase = prefixFailMsg "Let floating from Case failed: " $
  do flags <- constT getDynFlags
     caseT letVarsT idR idR (const idR) $ \x _ _ _ -> mapM (lookupDef . showPpr flags) x
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
     castT letVarsT idR $ \x co -> mapM (lookupDef . showPpr flags) x
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
        constT $ mapM (lookupDef . showPpr flags . fst) binds
        let (unfloatable, floatable) = filterBinds' args binds
        if null floatable
        then fail "no floatable let binds"
        else return (Let (Rec floatable) (Case scr wild ty (pre ++
           (con, args, if null unfloatable then body else Let (Rec unfloatable) body) : post)))
      getLets pre [] = fail "no memo lets found"
      getLets pre (alt : post) =
        (getLet pre post alt) <+ getLets (pre ++ [alt]) post
  getLets [] alts

