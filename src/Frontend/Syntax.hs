module Frontend.Syntax (Syntax (..), SyntaxKind (..), FunBind (..)) where

import Name (Id)

-- TODO: monomorphic
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
  | Not (Syntax a)
  | Neg (Syntax a)
  | Add (Syntax a) (Syntax a)
  | Sub (Syntax a) (Syntax a)
  | FNeg (Syntax a)
  | FAdd (Syntax a) (Syntax a)
  | FSub (Syntax a) (Syntax a)
  | FMul (Syntax a) (Syntax a)
  | FDiv (Syntax a) (Syntax a)
  | Eq (Syntax a) (Syntax a)
  | Le (Syntax a) (Syntax a)
  | If (Syntax a) (Syntax a) (Syntax a)
  | Let Id (Syntax a) (Syntax a)
  | Var Id
  | LetRec (FunBind a) (Syntax a)
  | App (Syntax a) [Syntax a]
  | Tuple [Syntax a]
  | LetTuple [Id] (Syntax a) (Syntax a)
  | Array (Syntax a) (Syntax a)
  | Get (Syntax a) (Syntax a)
  | Put (Syntax a) (Syntax a) (Syntax a)
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

data FunBind a = FunBind
  { name :: Id
  , args :: [Id]
  , rhs :: Syntax a
  }
  deriving stock (Eq, Show, Functor, Foldable, Traversable)
