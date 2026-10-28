module PrimOp (PrimOp (..), Cmp (..)) where

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
  | Get a a
  | Put a a a
  deriving stock (Show, Eq, Functor, Foldable, Traversable, Generic1)
  deriving (Show1) via Generically1 PrimOp

data Cmp = Eq | Le
  deriving stock (Show, Eq)
