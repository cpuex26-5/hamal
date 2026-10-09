module Span (Posn (..), Span (..), Spanned (..)) where

data Posn = Posn {offset, line, column :: Int}
  deriving stock (Eq, Show)

data Span = Span {start, stop :: Posn}
  deriving stock (Eq, Show)

instance Semigroup Span where
  Span start _ <> Span _ stop = Span start stop

data Spanned a = Spanned {value :: a, span :: Span}
  deriving stock (Eq, Show, Functor, Foldable, Traversable)
