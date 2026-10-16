module Frontend.SrcSpan (Posn (..), SrcSpan (..), Located (..)) where

data Posn = Posn {offset, line, column :: Int}
  deriving stock (Eq, Show)

data SrcSpan = SrcSpan {start, stop :: Posn}
  deriving stock (Eq, Show)

instance Semigroup SrcSpan where
  SrcSpan start _ <> SrcSpan _ stop = SrcSpan start stop

data Located a = Located {value :: a, span :: SrcSpan}
  deriving stock (Eq, Show, Functor, Foldable, Traversable)
