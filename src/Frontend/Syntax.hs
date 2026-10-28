module Frontend.Syntax (ExpF (..), FunBind (..), Parsed) where

import Control.Comonad.Cofree (Cofree (..))
import Data.Functor.Classes (Show1)
import Frontend.SrcSpan (SrcSpan)
import GHC.Generics (Generic1)
import Generic.Data (Generically1 (..))
import Name (Symbol)
import PrimOp (Cmp, PrimOp)

data ExpF b r
  = Unit
  | Bool Bool
  | Int Int
  | Float Float
  | Var Symbol
  | PrimOp (PrimOp r)
  | Not r
  | Cmp Cmp r r
  | If r r r
  | Let b r r
  | LetRec (FunBind b r) r
  | LetTuple [b] r r
  | App r [r]
  | Array r r
  deriving stock (Show, Eq, Functor, Foldable, Traversable, Generic1)
  deriving (Show1) via Generically1 (ExpF b)

data FunBind b r = FunBind {binder :: b, args :: [b], rhs :: r}
  deriving stock (Show, Eq, Functor, Foldable, Traversable, Generic1)
  deriving (Show1) via Generically1 (FunBind b)

type Parsed = Cofree (ExpF Symbol) SrcSpan
