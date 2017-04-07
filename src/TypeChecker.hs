module TypeChecker where

import AbsSyn

import Control.Monad.State.Strict
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Debug.Trace

floatTy :: Type
floatTy = TyCon "Float"

intTy :: Type
intTy = TyCon "Int"

boolTy :: Type
boolTy = TyCon "Bool"

infixr 1 -->
(-->) :: Type -> Type -> Type
t1 --> t2 = TyFun t1 t2

knownTypes :: [(String, Int)]
knownTypes = [("Int",0), ("Char",0), ("String",0), ("Float", 0), ("Bool", 0)]

knownFunctions :: [(String, Qual)]
knownFunctions =
  [ ("cos", [] :=> floatTy --> floatTy)
  , ("sin", [] :=> floatTy --> floatTy)
  , ("pi",  [] :=> floatTy)
  , ("itof", [] :=> intTy --> floatTy)
  , ("True", [] :=> boolTy)
  , ("False", [] :=> boolTy)
  , ("eqI", [] :=> intTy --> intTy --> boolTy)
  , ("ltI", [] :=> intTy --> intTy --> boolTy)
  , ("plusI", [] :=> intTy --> intTy --> intTy)
  , ("plus", [] :=> floatTy --> floatTy --> floatTy)
  , ("times", [] :=> floatTy --> floatTy --> floatTy)
  , ("div", [] :=> floatTy --> floatTy --> floatTy)
  ]

-- Check kinds
-- check top binds
-- infer lets
typecheck :: Module -> Module
typecheck m = evalState action env `seq` m
  where
    env = Env
      { envUnique = 0
      , envKinds = Map.fromList knownTypes
      , envScope = Map.fromList knownFunctions
      , envSubst = Map.empty
      }
    action = do
      mapM_ checkDataDecl (modDataDecls m)
      mapM_ checkFunDecl (modFunDecls m)

infixr 0 :=>
data Qual = [TyVar] :=> Type
  deriving (Eq, Show)

data Env = Env
  { envUnique :: Int
  , envKinds :: Map String Int -- Arity of types
  , envScope :: Map String Qual
  , envSubst :: Map TyVar Type
  }
type M a = State Env a

simpleQual :: Type -> Qual
simpleQual ty = tv ty :=> ty

insertType :: String -> Qual -> M ()
insertType name ty =
  modify $ \st -> st
    { envScope = Map.insert name ty (envScope st) }

withTypes :: [(String, Qual)] -> M a -> M a
withTypes [] = id
withTypes ((k,v):xs) = withType k v . withTypes xs

withType :: String -> Qual -> M a -> M a
withType name ty action = do
  mbPrevTy <- gets (Map.lookup name . envScope)
  insertType name ty
  ret <- action
  case mbPrevTy of
    Nothing -> modify $ \st -> st
      { envScope = Map.delete name (envScope st) }
    Just prev -> modify $ \st -> st
      { envScope = Map.insert name prev (envScope st) }
  return ret


newSubst :: TyVar -> Type -> M ()
newSubst tyvar ty =
  -- trace ("New subst: " ++ show tyvar ++ " -> " ++ show ty)
  modify $ \st -> st
    { envSubst = Map.insert tyvar ty (envSubst st) }

tv :: Type -> [TyVar]
tv (TyVar var) = [var]
tv TyCon{} = []
tv (TyFun t1 t2) = nub $ tv t1 ++ tv t2
tv (TyApp t1 t2) = nub $ tv t1 ++ tv t2

newTyVar :: TyVar -> M TyVar
newTyVar var = do
  st <- get
  let u = envUnique st
  put st{envUnique = u+1}
  return $ "new_" ++ var ++ "_" ++ show u

newAnyType :: M Type
newAnyType = TyVar <$> newTyVar "any"


instantiate :: Qual -> M Type
instantiate (tyvars :=> ty) = do
    vars' <- mapM newTyVar tyvars
    return $ replace (zip tyvars vars') ty

replace lst ty =
  case ty of
    TyVar var ->
      case lookup var lst of
        Nothing     -> TyVar var
        Just newVar -> TyVar newVar
    TyCon{} -> ty
    TyFun t1 t2 -> TyFun (replace lst t1) (replace lst t2)
    TyApp t1 t2 -> TyApp (replace lst t1) (replace lst t2)

normalize :: Qual -> Qual
normalize (tyvars :=> ty) = newVars :=> replace subst ty
  where
    newVars = map show [1 .. length tyvars]
    subst = zip tyvars newVars

qualFreeTyVars :: Qual -> [TyVar]
qualFreeTyVars (bound :=> ty) = tv ty \\ bound

generalize :: Type -> M Qual
generalize ty = do
  tys <- gets (Map.elems . envScope)
  let free = nub $ concatMap qualFreeTyVars tys
      gen = tv ty \\ free
  return $ gen :=> ty

-- Lookup and instantiate type.
freshType :: String -> M Type
freshType ident = do
  qual <- gets $ \st -> Map.findWithDefault err ident (envScope st)
  instantiate qual
  where
    err = error $ "Ident not found: " ++ ident

-- Apply substitutions
apply :: Type -> M Type
apply ty = do
    s <- gets envSubst
    return $ worker s ty
  where
    worker s (TyVar var) =
      case Map.lookup var s of
        Nothing -> TyVar var
        Just newTy -> worker s newTy
    worker s (TyCon con) = TyCon con
    worker s (TyFun t1 t2) = TyFun (worker s t1) (worker s t2)
    worker s (TyApp t1 t2) = TyApp (worker s t1) (worker s t2)

-- = TyVar TyVar
-- | TyCon TypeName
-- | TyFun Type Type
-- | TyApp Type Type
mgu :: Type -> Type -> M ()
mgu (TyApp l r) (TyApp l' r') = do
  mgu l l'
  join $ liftM2 mgu (apply r) (apply r')
mgu (TyFun l r) (TyFun l' r') = do
  mgu l l'
  join $ liftM2 mgu (apply r) (apply r')
mgu (TyVar u) t = varBind u t
mgu t (TyVar u) = varBind u t
mgu (TyCon tc1) (TyCon tc2) | tc1 == tc2 = return ()
mgu t1 t2 = error $ "Unification failure: " ++ show (t1,t2)

varBind :: TyVar -> Type -> M ()
varBind u t
  | t == TyVar u  = return ()
  | u `elem` tv t = error $ "occurs check failed: " ++ show (u, t)
  | otherwise     = newSubst u t

unify :: Type -> Type -> M ()
unify t1 t2 = do
  -- t1' <- apply t1
  -- t2' <- apply t2
  -- trace ("Unifying: " ++ show (t1', t2')) $ mgu t1' t2'
  join $ liftM2 mgu (apply t1) (apply t2)

checkDataDecl :: DataDecl -> M ()
checkDataDecl (DataDecl name args cons) = do
  modify $ \st -> st{envKinds = Map.insert name (length args) (envKinds st) }
  let retTy = foldl TyApp (TyCon name) (map TyVar args)
  mapM_ (checkConstructor retTy) cons

checkConstructor :: Type -> DataConstructor -> M ()
checkConstructor retType (DataConstructor name fields) = do
  insertType name (simpleQual $ foldr TyFun retType fields)
  mapM_ checkType fields

-- Check validity of type and return it's arity.
checkType :: Type -> M Int
checkType ty =
  case ty of
    TyVar{} -> return 0
    TyCon ty -> gets $ \st -> envKinds st Map.! ty
    TyFun t1 t2 -> do
      a1 <- checkType t1
      when (a1/=0) $ error $ "Higher kinded types not allowed: " ++ show t1
      a2 <- checkType t2
      when (a2/=0) $ error $ "Higher kinded types not allowed: " ++ show t2
      return 0
    TyApp t1 t2 -> do
      a1 <- checkType t1
      when (a1==0) $ error $ "Arity 0 in TyApp: " ++ show (t1, t2)
      a2 <- checkType t2
      when (a2/=0) $ error $ "Higher kinded types not allowed: " ++ show (t1, t2)
      return (a1-1)

checkFunDecl :: FunDecl -> M ()
checkFunDecl (FunDecl name ty args body) = do
  let qual = simpleQual ty
  checkType ty
  insertType name qual
  let expr = foldr Lam body args
  exprTy <- inferType expr
  fnInstTy <- instantiate qual
  unify fnInstTy exprTy

  exprTy' <- apply exprTy
  genTy <- generalize =<< apply exprTy
  when (normalize (simpleQual ty) /= normalize genTy) $
    error $ "Type error: " ++ show (normalize (simpleQual ty)) ++ "\n" ++ show (normalize genTy) ++ "\n" ++ show exprTy' ++ "\n" ++ show exprTy

inferType :: Expr -> M Type
inferType expr =
  case expr of
    LitChar{}   -> pure $ TyCon "Char"
    LitInt{}    -> pure intTy
    LitFloat{}  -> pure floatTy
    LitString{} -> pure $ TyCon "String"
    XmlNode _tag _props exprs -> do
      let t = TyCon "Svg"
      mapM_ inferType exprs
      return t
    Let lhs rhs body -> do
      rhsTy <- inferType rhs
      genTy <- generalize rhsTy
      withType lhs genTy $
        inferType body
    App a b -> do
      aTy <- inferType a
      bTy <- inferType b
      t <- newAnyType
      unify aTy (bTy `TyFun` t)
      return t
    Var var -> freshType var
    Con con -> freshType con
    Lam var body -> do
      varTy <- newAnyType
      withType var ([] :=> varTy) $ do
        bodyTy <- inferType body
        return (varTy `TyFun` bodyTy)
    Case scrut alts -> do
      retTy <- newAnyType
      scrutTy <- inferType scrut
      forM_ alts $ \(Alt pattern branch) -> do
        case pattern of
          PatternChar{} -> do
            unify scrutTy (TyCon "Char")
            unify retTy =<< inferType branch
          PatternInt{} -> do
            unify scrutTy (TyCon "Char")
            unify retTy =<< inferType branch
          PatternNode con args -> do
            argTypes <- replicateM (length args) newAnyType
            conRet <- newAnyType
            conTy <- freshType con
            unify (foldr TyFun conRet argTypes) conTy
            unify conRet scrutTy
            withTypes (zip args (map ([] :=>) argTypes)) $
              unify retTy =<< inferType branch
      return retTy


{-

data Tuple a b = Tuple a b

let x = Tuple 10
in x 20

instantiate: (new_a -> new_b -> Tuple new_a new_b)
app:
  split: (new_a) + (new_b -> Tuple new_a new_b)
  unify: new_a + Int
out:
  type: new_b -> Tuple new_a new_b
  subst: new_a = Int
  generalize: forall b. b -> Tuple Int b

Tuple :: forall a b. a -> b -> Tuple a b
x :: forall a. a -> Tuple Int a






fn :: a -> Tuple a Int
fn x = let y = Tuple x
       in y 10

y :: forall new_b. new_b -> Tuple a new_b

-}
