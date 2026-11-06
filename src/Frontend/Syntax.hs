module Frontend.Syntax (ExpF (..), Parsed, Typed) where

import Basic (Binder, Cmp, FunBind, PrimOp)
import Control.Comonad.Cofree (Cofree (..))
import Data.Bifunctor.TH (deriveBifoldable, deriveBifunctor, deriveBitraversable)
import Data.Functor.Classes (Show1)
import Frontend.SrcSpan (SrcSpan)
import GHC.Generics (Generic1)
import Generic.Data (Generically1 (..))
import Name (Symbol)
import Type (Type)

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
  | Let (Binder Symbol t) r r
  | LetRec (FunBind (Binder Symbol t) r) r
  | LetTuple [Binder Symbol t] r r
  | App r [r]
  deriving stock (Show, Eq, Functor, Foldable, Traversable, Generic1)
  deriving (Show1) via Generically1 (ExpF t)

type Parsed = Cofree (ExpF ()) SrcSpan

type Typed = Cofree (ExpF Type) (SrcSpan, Type)

deriveBifunctor ''ExpF
deriveBifoldable ''ExpF
deriveBitraversable ''ExpF
