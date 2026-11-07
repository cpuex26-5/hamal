module Type (TypeF (..), Type (..)) where

import Control.Monad (ap)
import Data.Void (Void)

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
