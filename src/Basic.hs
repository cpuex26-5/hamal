module Basic (PrimOp (..), Rel (..), Cmp (..), Binder (..), FunBind (..)) where

import Data.Bifunctor.TH (deriveBifoldable, deriveBifunctor, deriveBitraversable)
import Data.Functor.Classes (Show1)
import GHC.Generics (Generic1)
import Generic.Data (Generically1 (..))
import Prettyprinter (Pretty (..), tupled, (<+>))

data PrimOp a
  = Not a
  | Neg a
  | Add a a
  | Sub a a
  | Sali a Int
  | Sari a Int
  | FNeg a
  | FAdd a a
  | FSub a a
  | FMul a a
  | FDiv a a
  | FSqrt a
  | FAbs a
  | Floor a
  | Tuple [a]
  | Array a a
  | Get a a
  | Put a a a
  | FloatOfInt a
  | IntOfFloat a
  | In a
  | Out a
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable, Generic1)
  deriving (Show1) via Generically1 PrimOp

instance (Pretty a) => Pretty (PrimOp a) where
  pretty = \case
    Not e -> "not" <+> pretty e
    Neg e -> "-" <+> pretty e
    Add l r -> pretty l <+> "+" <+> pretty r
    Sub l r -> pretty l <+> "-" <+> pretty r
    Sali e shamt -> pretty e <+> "<<" <+> pretty shamt
    Sari e shamt -> pretty e <+> "/" <+> pretty shamt
    FNeg e -> "-." <+> pretty e
    FAdd l r -> pretty l <+> "+." <+> pretty r
    FSub l r -> pretty l <+> "-." <+> pretty r
    FMul l r -> pretty l <+> "*." <+> pretty r
    FDiv l r -> pretty l <+> "/." <+> pretty r
    FSqrt e -> "sqrt" <+> pretty e
    FAbs e -> "fabs" <+> pretty e
    Floor e -> "floor" <+> pretty e
    Tuple es -> tupled (pretty <$> es)
    Array n x -> "Array.make" <+> pretty n <+> pretty x
    Get arr idx -> pretty arr <> ".(" <> pretty idx <> ")"
    Put arr idx rhs -> pretty arr <> ".(" <> pretty idx <> ")" <+> "<-" <+> pretty rhs
    FloatOfInt e -> "float_of_int" <+> pretty e
    IntOfFloat e -> "int_of_float" <+> pretty e
    In e -> "read_byte" <+> pretty e
    Out e -> "print_byte" <+> pretty e

data Rel = Eq | Neq | Lt | Le | Gt | Ge
  deriving stock (Show, Eq, Ord)

instance Pretty Rel where
  pretty Eq = "="
  pretty Neq = "<>"
  pretty Lt = "<"
  pretty Le = "<="
  pretty Gt = ">"
  pretty Ge = ">="

data Cmp = ICmp Rel | FCmp Rel
  deriving stock (Show, Eq, Ord)

instance Pretty Cmp where
  pretty = \case
    ICmp rel -> pretty rel
    FCmp rel -> pretty rel <> "."

data Binder n t = Binder {name :: n, ty :: t}
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

instance (Pretty n, Pretty t) => Pretty (Binder n t) where
  pretty Binder {name, ty} = pretty name <+> ":" <+> pretty ty

deriveBifunctor ''Binder
deriveBifoldable ''Binder
deriveBitraversable ''Binder

data FunBind b r = FunBind {binder :: b, params :: [b], rhs :: r}
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable, Generic1)
  deriving (Show1) via Generically1 (FunBind b)

deriveBifunctor ''FunBind
deriveBifoldable ''FunBind
deriveBitraversable ''FunBind
