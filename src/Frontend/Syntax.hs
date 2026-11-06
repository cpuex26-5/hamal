module Frontend.Syntax (ExpF (..), Binder (..), FunBind (..), Parsed, Typed) where

import Basic (Cmp, PrimOp)
import Control.Comonad.Cofree (Cofree (..))
import Data.Bifunctor.TH (deriveBifoldable, deriveBifunctor, deriveBitraversable)
import Data.Functor.Classes (Show1)
import Frontend.SrcSpan (SrcSpan)
import GHC.Generics (Generic1)
import Generic.Data (Generically1 (..))
import Name (Symbol)
import Type (Type)

data Binder t = Binder {name :: Symbol, ty :: t}
  deriving stock (Show, Eq, Functor, Foldable, Traversable, Generic1)
  deriving (Show1) via Generically1 Binder

data ExpF t r
  = Unit
  | Bool Bool
  | Int Int
  | Float Float
  | Var Symbol
  | PrimOp (PrimOp r)
  | Not r
  | Cmp Cmp r r
  | If r r r
  | Let (Binder t) r r
  | LetRec (FunBind (Binder t) r) r
  | LetTuple [Binder t] r r
  | App r [r]
  deriving stock (Show, Eq, Functor, Foldable, Traversable, Generic1)
  deriving (Show1) via Generically1 (ExpF t)

data FunBind b r = FunBind {binder :: b, args :: [b], rhs :: r}
  deriving stock (Show, Eq, Functor, Foldable, Traversable, Generic1)
  deriving (Show1) via Generically1 (FunBind b)

type Parsed = Cofree (ExpF ()) SrcSpan

type Typed = Cofree (ExpF Type) (SrcSpan, Type)

deriveBifunctor ''FunBind
deriveBifoldable ''FunBind
deriveBitraversable ''FunBind
deriveBifunctor ''ExpF
deriveBifoldable ''ExpF
deriveBitraversable ''ExpF
