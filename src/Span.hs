module Span (Posn (..), Span (..), Spanned (..)) where

data Posn = Posn
  { offset, line, column :: Int
  }
  deriving stock (Eq, Show)

data Span = Span
  { start :: Posn
  , stop :: Posn
  }
  deriving stock (Eq, Show)

data Spanned a = Spanned
  { value :: a
  , span :: Span
  }
  deriving stock (Eq, Show)
