module Name (Id, Label, Fresh, toLabel, refresh, runFresh, external, internal) where

import Buildable (Buildable (..))
import Data.ByteString.Builder (intDec, shortByteString)
import Data.ByteString.Short (ShortByteString)
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, (:>))
import Effectful.Dispatch.Static (SideEffects (..), StaticRep, evalStaticRep, stateStaticRep)

data NameSpace = Local | Global

data NameSort = Internal Int | External
  deriving stock (Eq, Ord)

data Name (s :: NameSpace)
  = Name {hint :: ShortByteString, sort :: NameSort}
  deriving stock (Eq, Ord)

instance Show (Name s) where
  show Name {hint, sort} = case sort of
    Internal n -> show hint <> "." <> show n
    External -> show hint

instance Buildable (Name s) where
  build Name {hint, sort} = case sort of
    Internal n -> shortByteString hint <> "." <> intDec n
    External -> shortByteString hint

type Id = Name Local

type Label = Name Global

internal :: ShortByteString -> Name s
internal hint = Name {hint, sort = Internal 0}

external :: ShortByteString -> Name s
external hint = Name {hint, sort = External}

toLabel :: Id -> Label
toLabel (Name {..}) = Name {..}

data Fresh :: Effect

type instance DispatchOf Fresh = Static NoSideEffects

newtype instance StaticRep Fresh = Counter Int

runFresh :: Eff (Fresh : es) a -> Eff es a
runFresh = evalStaticRep (Counter 1)

refresh :: (Fresh :> es) => Name s -> Eff es (Name s)
refresh name = stateStaticRep \(Counter n) -> (name {sort = Internal n}, Counter (n + 1))
