module Type (TypeF (..), Type (..)) where

import Control.Monad (ap)
import Data.Void (Void)
import Prettyprinter (Doc, Pretty (..), concatWith, (<+>))

data TypeF v
  = Unit
  | Bool
  | Int
  | Float
  | Tuple [TypeF v]
  | Fun [TypeF v] (TypeF v)
  | Array (TypeF v)
  | Var v
  deriving stock (Show, Eq, Ord, Functor, Foldable, Traversable)

instance Applicative TypeF where
  pure = Var
  (<*>) = ap

instance Monad TypeF where
  t >>= k = case t of
    Unit -> Unit
    Bool -> Bool
    Int -> Int
    Float -> Float
    Tuple tys -> Tuple ((>>= k) <$> tys)
    Fun args body -> Fun ((>>= k) <$> args) (body >>= k)
    Array ty -> Array (ty >>= k)
    Var ty -> k ty

newtype Type = Type (TypeF Void)
  deriving newtype (Show, Eq, Ord)

instance Pretty Type where
  pretty (Type t) = pTypeF 0 t

-- 0(->), 1(*), 2(array)
pTypeF :: Int -> TypeF Void -> Doc ann
pTypeF prec = \case
  Unit -> "unit"
  Bool -> "bool"
  Int -> "int"
  Float -> "float"
  Tuple tys -> parensIf (prec > 1) $ concatWith (\a b -> a <+> "*" <+> b) (pTypeF 2 <$> tys)
  Fun args ret -> parensIf (prec > 0) $ concatWith (\a b -> a <+> "->" <+> b) ((pTypeF 1 <$> args) <> [pTypeF 0 ret])
  Array ty -> pTypeF 2 ty <+> "array"
  where
    parensIf requires doc = case requires of
      True -> "(" <> doc <> ")"
      False -> doc
