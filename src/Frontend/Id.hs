module Frontend.Id (Id (..), Fresh, buildId, refresh, freshTmpId, runFresh) where

import Control.Monad.Trans.State (State, evalState, get, modify)
import Data.ByteString.Builder (intDec, lazyByteString, toLazyByteString)
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy.Char8 qualified as BS

newtype Id = Id BS.ByteString
  deriving newtype (Eq, Ord, Show)

buildId :: Id -> BB.Builder
buildId (Id ident) = lazyByteString ident

type Fresh = State Int

runFresh :: Fresh a -> a
runFresh m = evalState m 0

refresh :: Id -> Fresh Id
refresh ident = do
  counter <- get
  modify (+ 1)
  pure . Id . toLazyByteString $ buildId ident <> "." <> intDec counter

freshTmpId :: Fresh Id
freshTmpId = refresh (Id "T")
