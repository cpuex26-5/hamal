module Syntax (Name (..), Expr (..), ExprKind (..), FunBind (..)) where

import Data.ByteString.Lazy.Char8 qualified as BS
import Type (Type)

data Name = Name BS.ByteString
  deriving stock (Eq, Show)

data Expr a = Expr {ann :: a, kind :: ExprKind a}
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

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
  | Let (Name, Type) (Expr a) (Expr a)
  | Var Name
  | LetRec (FunBind a) (Expr a)
  | App (Expr a) [(Expr a)]
  | Tuple [(Expr a)]
  | LetTuple [(Name, Type)] (Expr a) (Expr a)
  | Array (Expr a) (Expr a)
  | Get (Expr a) (Expr a)
  | Put (Expr a) (Expr a) (Expr a)
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

data FunBind a = FunBind
  { name :: (Name, Type)
  , args :: [(Name, Type)]
  , body :: Expr a
  }
  deriving stock (Eq, Show, Functor, Foldable, Traversable)
