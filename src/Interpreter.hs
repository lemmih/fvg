module Interpreter (runScript) where

import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Parsec

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

-- to whnf
evalExpr :: Expr -> Eval Expr
evalExpr expr =
  case expr of
    LitChar{}     -> pure expr
    LitString str -> LitString <$> interpolate str
    LitInt{}      -> pure expr
    XmlNode tag props exprs ->
      XmlNode tag props <$> mapM evalExpr exprs
    Let lhs rhs body ->
      local (Map.insert lhs rhs) (evalExpr body)
    App a b -> do
      a' <- evalExpr a
      case a' of
        Lam var body -> evalExpr $ Let var b body
        _ -> error "App/Lam mismatch."
    Var var -> do
      mbSubst <- asks (Map.lookup var)
      case mbSubst of
        Nothing  -> pure $ Var var
        Just val -> evalExpr val
    Con con -> pure (Con con)
    Lam var body -> Lam var <$> evalExpr body
    Case scrut alts -> do
      scrut' <- evalExpr scrut
      findAlternative scrut' alts
    -- _ -> error $ "Unhandled evalExpr: " ++ show expr

findAlternative :: Expr -> [Alt] -> Eval Expr
findAlternative _ [] = error "No valid case branch"
findAlternative expr (Alt pattern branch:xs) =
  case (expr, pattern) of
    (LitChar c, PatternChar c') | c==c' -> evalExpr branch
    (LitInt i, PatternInt i') | i==i' -> evalExpr branch
    (_, PatternNode pCon pArgs)
      | Just (con, args) <- getCon [] expr
      , pCon == con
      -> local (\m -> foldl (\m (k,v) -> Map.insert k v m) m (zip pArgs args)) $
          evalExpr branch
    _ -> findAlternative expr xs
  where
    getCon acc (App a b) = getCon (b:acc) a
    getCon acc (Con con) = Just (con, reverse acc)
    getCon _ _ = Nothing


interpolate :: FvgString -> Eval FvgString
interpolate inp = concat <$> mapM worker inp
  where
    worker (TextBlock txt) = pure [TextBlock txt]
    worker (VariableBlock var) = do
      mbExpr <- asks (Map.lookup var)
      case mbExpr of
        Nothing -> pure [VariableBlock var]
        Just expr -> do
          expr' <- evalExpr expr
          case expr' of
            LitString txt -> pure txt
            LitChar c     -> pure [TextBlock (show c)]
            LitInt i      -> pure [TextBlock (show i)]
            _             -> pure [TextBlock (show expr')]
