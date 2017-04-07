module AbsSyn where

import Data.List

type TyVar = String
type TypeName = String
type FnName = String

data Module = Module
  { modDataDecls :: [DataDecl]
  , modFunDecls :: [FunDecl]
  } deriving (Show)

data DataDecl = DataDecl
  { ddName          :: TypeName
  , ddTypeVariables :: [TyVar]
  , ddConstructors  :: [DataConstructor]
  } deriving (Show)

data DataConstructor = DataConstructor
  { dcName   :: TypeName
  , dcFields :: [Type]
  } deriving (Show)

data Type
    = TyVar TyVar
    | TyCon TypeName
    | TyFun Type Type
    | TyApp Type Type
    deriving (Show, Eq)

data FunDecl = FunDecl
  { fdName :: FnName
  , fdType :: Type
  , fdArguments :: [String]
  , fdBody :: Expr
  } deriving (Show)

data Expr
  = LitChar Char
  | LitInt Integer
  | LitFloat Double
  | LitString FvgString
  | XmlNode String [(String,FvgString)] [Expr]
  | Let String Expr Expr
  | App Expr Expr
  | Var String
  | Con String
  | Lam String Expr
  | Case Expr [Alt]

arrowPrecedence = 1
appPrecedence = 2

parensIf False x = x
parensIf True x = showChar '(' . x . showChar ')'

instance Show Expr where
  showsPrec p expr =
    case expr of
      LitChar c -> showsPrec p c
      LitInt i  -> showsPrec p i
      LitFloat f -> showsPrec p f
      LitString s -> showSvgString s
      XmlNode tag props exprs -> parensIf (p > 0) $
        showString " <" . showString tag . showChar ' ' .
        showProps props . showString ">" .
        flip (foldr id) ({-intersperse (showString "; ")-} (map shows exprs)) .
        showString (" </"++tag++"> ")
      Let lhs rhs body ->
        showString "let " . showString lhs . showString " = " . shows rhs . showString " in " . shows body
      App a b -> parensIf (p > arrowPrecedence) $
        showsPrec arrowPrecedence a . showChar ' ' . showsPrec appPrecedence b
      Var s -> showString s
      Con c -> showString c
      Lam bind body -> parensIf (p > 0) $
        showChar '\\' . showString bind . showString " -> " . shows body

showProps [] = id
showProps ((k,v):xs) = showString k . showChar '=' . showSvgString v . showChar ' ' . showProps xs

showSvgString = shows . concatMap toString
  where
    toString (TextBlock txt) = txt
    toString (VariableBlock var) = "{{" ++ var ++ "}}"
    toString ExprBlock{} = "{{__EXPR__}}"

data Alt = Alt Pattern Expr deriving (Show)

data Pattern
  = PatternChar Char
  | PatternInt Integer
  | PatternNode String [String]
  deriving (Show)

-- "string {{variable}} more string"
type FvgString = [StringBlock]
data StringBlock = TextBlock String | VariableBlock String | ExprBlock Expr
  deriving (Show)
