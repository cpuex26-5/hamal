module Frontend.Id (Id (..), Counter, tmpId, initialCounter) where

import Control.Monad.Trans.State (State, get, modify)
import Data.ByteString.Builder (intDec, toLazyByteString)
import Data.ByteString.Lazy.Char8 qualified as BS

newtype Id = Id BS.ByteString
  deriving newtype (Eq, Show)

newtype Counter = Counter Int

initialCounter :: Counter
initialCounter = Counter 0

increment :: Counter -> Counter
increment (Counter cnt) = Counter (cnt + 1)

tmpId :: State Counter Id
tmpId = do
  modify increment
  Counter cnt <- get
  pure . Id . toLazyByteString $ "T" <> intDec cnt
