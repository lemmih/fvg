{-# LANGUAGE TupleSections #-}
module Interpreter (runScript) where

import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Parsec
import Debug.Trace

import AbsSyn
import Parser
import TypeChecker

runScript :: String -> Expr
runScript inp = evaluate (typecheck (runParse (parseModule <* eof) inp))

evaluate :: Module -> Expr
evaluate m = runReader (evalFunction "main") env
  where
    env = Map.fromList
      [ (fdName fun, fdLambda fun) | fun <- modFunDecls m ]

fdLambda :: FunDecl -> Expr
fdLambda fd = foldr Lam (fdBody fd) (fdArguments fd)

type Eval a = Reader (Map String Expr) a

require :: (Ord k, Show k) => k -> Map k v -> v
require k m =
  case Map.lookup k m of
    Just v  -> v
    Nothing -> error $ "Key not found: " ++ show (k, Map.keys m)

evalFunction :: String -> Eval Expr
evalFunction name = evalExpr =<< asks (require name)

substMany :: [(String, Expr)] -> Expr -> Expr
substMany [] = id
substMany ((k,v):xs) = substMany xs . subst k v

subst :: String -> Expr -> Expr -> Expr
subst key value expr =
  case expr of
    LitChar{} -> expr
    LitInt{}  -> expr
    LitFloat{} -> expr
    LitString txt -> LitString $ map substBlock txt
    XmlNode tag props exprs ->
      XmlNode tag [ (k, map substBlock v) | (k,v) <- props]
                  (map (subst key value) exprs)
    Let lhs rhs body ->
      Let lhs
        (subst key value rhs)
        (if lhs==key then body else subst key value body)
    App a b -> App (subst key value a) (subst key value b)
    Var var
      | var == key -> value
      | otherwise  -> expr
    Con{} -> expr
    Lam var body
      | var == key -> Lam var body
      | otherwise  -> Lam var (subst key value body)
    Case scrut alts ->
      Case (subst key value scrut)
        [ case pattern of
            PatternNode _ args
              | key `elem` args -> Alt pattern branch
            _                   -> Alt pattern (subst key value branch)
        | Alt pattern branch <- alts ]
  where
    substBlock (VariableBlock var)
      | var == key = ExprBlock value
      | otherwise  = VariableBlock var
    substBlock (TextBlock txt) = TextBlock txt
    substBlock (ExprBlock e) = ExprBlock (subst key value e)


binOpInt a b fn = do
  a' <- evalExpr a
  b' <- evalExpr b
  case (a', b') of
    (LitInt aI, LitInt bI) -> pure (fn aI bI)
    _ -> error $ "Expected two integer arguments"

unOpInt a fn = do
  a' <- evalExpr a
  case a' of
    LitInt aI -> pure (fn aI)
    _ -> error $ "Expected single integer argument"

binOpFloat a b fn = do
  a' <- evalExpr a
  b' <- evalExpr b
  case (a', b') of
    (LitFloat aF, LitFloat bF) -> pure (fn aF bF)
    _ -> error $ "Expected two float arguments"

unOpFloat a fn = do
  a' <- evalExpr a
  case a' of
    LitFloat aF -> pure (fn aF)
    _ -> error $ "Expected single float argument"

evalExpr :: Expr -> Eval Expr
evalExpr expr = -- trace ("Eval: " ++ show expr  )$
  case expr of
    LitChar{}     -> pure expr
    LitString str -> LitString <$> interpolate str
    LitInt{}      -> pure expr
    LitFloat{}    -> pure expr
    XmlNode tag props exprs ->
      XmlNode tag <$> mapM evalProp props
                  <*> (concatMap flattenExpr <$> mapM evalExpr exprs)
    Let lhs rhs body -> evalExpr (subst lhs rhs body)

    App (App (Var "ltI") a) b -> binOpInt a b $ \aI bI ->
      if aI < bI then Con "True" else Con "False"

    App (App (Var "plusI") a) b -> binOpInt a b $ \aI bI -> LitInt (aI+bI)
    App (App (Var "plus") a) b -> binOpFloat a b $ \aF bF -> LitFloat (aF+bF)
    App (App (Var "times") a) b -> binOpFloat a b $ \aF bF -> LitFloat (aF*bF)
    App (App (Var "div") a) b -> binOpFloat a b $ \aF bF -> LitFloat (aF/bF)

    App (Var "cos") a -> unOpFloat a $ \aF -> LitFloat (cos aF)
    App (Var "sin") a -> unOpFloat a $ \aF -> LitFloat (sin aF)
    App (Var "itof") a -> unOpInt a $ \aI -> LitFloat (fromIntegral aI)

    Var "pi" -> pure $ LitFloat pi

    App a b -> do
      a' <- evalExpr a
      case a' of
        Lam var body -> evalExpr $ Let var b body
        Con{} -> App a' <$> evalExpr b
        App{} -> App a' <$> evalExpr b
        _ -> error $ "App/Lam mismatch: " ++ show a'
    Var var -> do
      mbSubst <- asks (Map.lookup var)
      case mbSubst of
        Nothing  -> error $ "Undefined var: " ++ var -- pure $ Var var
        Just val -> evalExpr val
    Con con -> pure (Con con)
    Lam var body -> pure $ Lam var body
    Case scrut alts -> do
      scrut' <- evalExpr scrut
      findAlternative scrut' alts
    -- _ -> error $ "Unhandled evalExpr: " ++ show expr

flattenExpr :: Expr -> [Expr]
flattenExpr expr =
  case expr of
    Con "Nil" -> []
    App (App (Con "Cons") a) b -> a : flattenExpr b
    _ -> [expr]

evalProp :: (String, FvgString) -> Eval (String, FvgString)
evalProp (key, value) = (key,) <$> interpolate value

findAlternative :: Expr -> [Alt] -> Eval Expr
findAlternative _ [] = error "No valid case branch"
findAlternative expr (Alt pattern branch:xs) =
  case (expr, pattern) of
    (LitChar c, PatternChar c') | c==c' -> evalExpr branch
    (LitInt i, PatternInt i') | i==i' -> evalExpr branch
    (_, PatternNode pCon pArgs)
      | Just (con, args) <- getCon [] expr
      , pCon == con
      -> evalExpr (substMany (zip pArgs args) branch)
    _ -> findAlternative expr xs
  where
    getCon acc (App a b) = getCon (b:acc) a
    getCon acc (Con con) = Just (con, acc)
    getCon _ _ = Nothing


interpolate :: FvgString -> Eval FvgString
interpolate inp = concat <$> mapM worker inp
  where
    worker (TextBlock txt) = pure [TextBlock txt]
    worker (ExprBlock expr) = do
      expr' <- evalExpr expr
      case expr' of
        LitString txt -> pure txt
        LitChar c     -> pure [TextBlock (show c)]
        LitInt i      -> pure [TextBlock (show i)]
        _             -> pure [TextBlock (show expr')]
    worker (VariableBlock var) = do
      mbExpr <- asks (Map.lookup var)
      case mbExpr of
        Nothing -> pure [VariableBlock var]
        Just expr -> worker (ExprBlock expr)
