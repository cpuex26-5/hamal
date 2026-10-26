module Frontend.Syntax (Syntax (..), SyntaxKind (..), FunBind (..)) where

import Name (Id)
import PrimOp (Cmp, PrimOp)

data Syntax a = Syntax {ann :: a, kind :: SyntaxKind a}
  deriving stock (Eq, Functor, Foldable, Traversable)

instance Show (Syntax a) where
  show Syntax {kind} = "(" <> show kind <> ")"

-- TODO: maybe better to use 'NonEmpty'

data SyntaxKind a
  = Unit
  | Bool Bool
  | Int Int
  | Float Float
  | Var Id
  | Not (Syntax a)
  | PrimOp (PrimOp (Syntax a))
  | Cmp Cmp (Syntax a) (Syntax a)
  | If (Syntax a) (Syntax a) (Syntax a)
  | Let Id (Syntax a) (Syntax a)
  | LetRec (FunBind a) (Syntax a)
  | LetTuple [Id] (Syntax a) (Syntax a)
  | App (Syntax a) [Syntax a]
  | Array (Syntax a) (Syntax a)
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

data FunBind a = FunBind
  { name :: Id
  , args :: [Id]
  , rhs :: Syntax a
  }
  deriving stock (Eq, Show, Functor, Foldable, Traversable)
