module AbsSyn where

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
  | LitString FvgString
  | XmlNode String [(String,FvgString)] [Expr]
  | Let String Expr Expr
  | App Expr Expr
  | Var String
  | Con String
  | Lam String Expr
  | Case Expr [Alt]
  deriving (Show)

data Alt = Alt Pattern Expr deriving (Show)

data Pattern
  = PatternChar Char
  | PatternInt Integer
  | PatternNode String [String]
  deriving (Show)

-- "string {{variable}} more string"
type FvgString = [StringBlock]
data StringBlock = TextBlock String | VariableBlock String
  deriving (Show)
