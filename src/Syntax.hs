module Syntax (Name (..), Expr (..), FunBind (..)) where

import Data.ByteString.Lazy.Char8 qualified as BS
import Type (Type)

newtype Name = Name BS.ByteString
  deriving newtype (Eq, Show)

data Expr
  = Unit
  | Bool Bool
  | Int Int
  | Float Float
  | Not Expr
  | Neg Expr
  | Add Expr Expr
  | Sub Expr Expr
  | FNeg Expr
  | FAdd Expr Expr
  | FSub Expr Expr
  | FMul Expr Expr
  | FDiv Expr Expr
  | Eq Expr Expr
  | Le Expr Expr
  | If Expr Expr Expr
  | Let (Name, Type) Expr Expr
  | Var Name
  | LetRec FunBind Expr
  | App Expr [Expr]
  | Tuple [Expr]
  | LetTuple [(Name, Type)] Expr Expr
  | Array Expr Expr
  | Get Expr Expr
  | Put Expr Expr Expr
  deriving stock (Eq, Show)

data FunBind = FunBind
  { name :: (Name, Type)
  , args :: [(Name, Type)]
  , body :: Expr
  }
  deriving stock (Eq, Show)
