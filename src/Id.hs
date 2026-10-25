module Id (Id, Label, Fresh, mkName, toLabel, fresh, refresh, runFresh) where

import Buildable (Buildable (..))
import Data.ByteString.Builder (intDec, shortByteString)
import Data.ByteString.Short (ShortByteString)
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, (:>))
import Effectful.Dispatch.Static (SideEffects (..), StaticRep, evalStaticRep, stateStaticRep)

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

data Fresh :: Effect

type instance DispatchOf Fresh = Static NoSideEffects

newtype instance StaticRep Fresh = Counter Int

runFresh :: Eff (Fresh : es) a -> Eff es a
runFresh = evalStaticRep (Counter 1)

refresh :: (Fresh :> es) => Name s -> Eff es (Name s)
refresh name = stateStaticRep \(Counter !n) -> (name {unique = n}, Counter (n + 1))

fresh :: (Fresh :> es) => ShortByteString -> Eff es (Name s)
fresh = refresh . mkName
