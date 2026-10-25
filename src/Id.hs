module Id (Id, Label, Fresh, mkName, toLabel, fresh, refresh, runFresh) where

import Buildable (Buildable (..))
import Control.Monad.Trans.State (State, evalState, get, modify)
import Data.ByteString.Builder (intDec, shortByteString)
import Data.ByteString.Short (ShortByteString)

data NameSort = Local | Global

data Name (s :: NameSort)
  = Name {hint :: ShortByteString, unique :: Int}
  deriving stock (Eq, Ord)

instance Show (Name s) where
  show Name {hint, unique} = show hint <> "." <> show unique

instance Buildable (Name s) where
  build Name {hint, unique} = shortByteString hint <> "." <> intDec unique

type Id = Name Local

type Label = Name Global

mkName :: ShortByteString -> Name s
mkName hint = Name {hint, unique = 0}

toLabel :: Id -> Label
toLabel (Name {..}) = Name {..}

type Fresh = State Int

runFresh :: Fresh a -> a
runFresh m = evalState m 0

fresh :: ShortByteString -> Fresh (Name s)
fresh hint = pure (Name {hint, unique = 0})

refresh :: Name s -> Fresh (Name s)
refresh name = do
  counter <- get
  modify (+ 1)
  pure $ name {unique = counter}
