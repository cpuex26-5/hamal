module Basic (PrimOp (..), Cmp (..), Binder (..), FunBind (..)) where

import Data.Bifunctor.TH (deriveBifoldable, deriveBifunctor, deriveBitraversable)
import Data.Functor.Classes (Show1)
import GHC.Generics (Generic1)
import Generic.Data (Generically1 (..))

data PrimOp a
  = Neg a
  | FNeg a
  | Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | FAdd a a
  | FSub a a
  | FMul a a
  | FDiv a a
  | Tuple [a]
  | Array a a
  | Get a a
  | Put a a a
  deriving stock (Show, Eq, Functor, Foldable, Traversable, Generic1)
  deriving (Show1) via Generically1 PrimOp

data Cmp = Eq | Le
  deriving stock (Show, Eq)

data Binder n t = Binder {name :: n, ty :: t}
  deriving stock (Show, Eq, Functor, Foldable, Traversable)

deriveBifunctor ''Binder
deriveBifoldable ''Binder
deriveBitraversable ''Binder

data FunBind b r = FunBind {binder :: b, args :: [b], rhs :: r}
  deriving stock (Show, Eq, Functor, Foldable, Traversable, Generic1)
  deriving (Show1) via Generically1 (FunBind b)

deriveBifunctor ''FunBind
deriveBifoldable ''FunBind
deriveBitraversable ''FunBind
