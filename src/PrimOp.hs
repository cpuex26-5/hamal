module PrimOp (PrimOp (..), Cmp (..)) where

data PrimOp a
  = Neg a
  | FNeg a
  | Add a a
  | Sub a a
  | FAdd a a
  | FSub a a
  | FMul a a
  | FDiv a a
  | Tuple [a]
  | Get a a
  | Put a a a
  deriving stock (Show, Eq, Functor, Foldable, Traversable)

data Cmp = Eq | Le
  deriving stock (Show, Eq)
