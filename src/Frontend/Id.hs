module Frontend.Id (Id (..), Counter, freshId, initialCounter, buildId) where

import Control.Monad.Trans.State (State, get, modify)
import Data.ByteString.Builder (intDec, lazyByteString, toLazyByteString)
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy.Char8 qualified as BS

newtype Id = Id BS.ByteString
  deriving newtype (Eq, Show)

buildId :: Id -> BB.Builder
buildId (Id ident) = lazyByteString ident

newtype Counter = Counter Int

initialCounter :: Counter
initialCounter = Counter 0

increment :: Counter -> Counter
increment (Counter cnt) = Counter (cnt + 1)

freshId :: State Counter Id
freshId = do
  modify increment
  Counter cnt <- get
  pure . Id . toLazyByteString $ "T" <> intDec cnt
