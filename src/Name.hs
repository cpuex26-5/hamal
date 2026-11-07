module Name (Symbol, Id, Label (..), Fresh, fresh, refresh, runFresh, freshI) where

import Buildable (Buildable (..))
import Data.ByteString.Builder (intDec, shortByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as SBS
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, (:>))
import Effectful.Dispatch.Static (SideEffects (..), StaticRep, evalStaticRep, stateStaticRep)
import Prettyprinter (Pretty (..))

type Symbol = ShortByteString

data Id = Id {unique :: Int, hint :: ShortByteString}
  deriving stock (Eq, Ord)

instance Show Id where
  show Id {unique, hint} = BS8.unpack (SBS.fromShort hint) <> "." <> show unique

instance Pretty Id where
  pretty Id {unique, hint} = pretty (BS8.unpack (SBS.fromShort hint)) <> "." <> pretty unique

data Label = Local Id | Global Symbol
  deriving stock (Show, Eq)

instance Pretty Label where
  pretty = \case
    Local ident -> pretty ident
    Global sym -> pretty (BS8.unpack (SBS.fromShort sym))

instance Buildable Label where
  build = \case
    Local Id {unique, hint} -> shortByteString hint <> intDec unique
    Global sym -> shortByteString sym

data Fresh :: Effect

type instance DispatchOf Fresh = Static NoSideEffects

newtype instance StaticRep Fresh = Counter Int

runFresh :: Eff (Fresh : es) a -> Eff es a
runFresh = evalStaticRep (Counter 1)

fresh :: (Fresh :> es) => Symbol -> Eff es Id
fresh hint = refresh Id {hint, unique = 0}

refresh :: (Fresh :> es) => Id -> Eff es Id
refresh name = stateStaticRep \(Counter n) -> (name {unique = n}, Counter (n + 1))

freshI :: (Fresh :> es) => Eff es Int
freshI = stateStaticRep \(Counter n) -> (n, Counter (n + 1))
