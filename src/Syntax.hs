module Syntax (Name (..), Expr (..), ExprKind (..), FunBind (..)) where

import Data.ByteString.Lazy.Char8 qualified as BS

data Name a = Name {ann :: a, name :: BS.ByteString}
  deriving stock (Eq, Functor, Foldable, Traversable)

instance Show (Name a) where
  show Name {name} = BS.unpack name

data Expr a = Expr {ann :: a, kind :: ExprKind a}
  deriving stock (Eq, Functor, Foldable, Traversable)

instance Show (Expr a) where
  show Expr {kind} = "(" <> show kind <> ")"

-- TODO: maybe better to use 'NonEmpty'

data ExprKind a
  = Unit
  | Bool Bool
  | Int Int
  | Float Float
  | Not (Expr a)
  | Neg (Expr a)
  | Add (Expr a) (Expr a)
  | Sub (Expr a) (Expr a)
  | FNeg (Expr a)
  | FAdd (Expr a) (Expr a)
  | FSub (Expr a) (Expr a)
  | FMul (Expr a) (Expr a)
  | FDiv (Expr a) (Expr a)
  | Eq (Expr a) (Expr a)
  | Le (Expr a) (Expr a)
  | If (Expr a) (Expr a) (Expr a)
  | Let (Name a) (Expr a) (Expr a)
  | Var (Name a)
  | LetRec (FunBind a) (Expr a)
  | App (Expr a) [Expr a]
  | Tuple [(Expr a)]
  | LetTuple [Name a] (Expr a) (Expr a)
  | Array (Expr a) (Expr a)
  | Get (Expr a) (Expr a)
  | Put (Expr a) (Expr a) (Expr a)
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

data FunBind a = FunBind
  { name :: Name a
  , args :: [Name a]
  , body :: Expr a
  }
  deriving stock (Eq, Show, Functor, Foldable, Traversable)
