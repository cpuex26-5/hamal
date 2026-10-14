module Frontend.Id (Id (..)) where

import Data.ByteString.Lazy.Char8 qualified as BS

newtype Id = Id BS.ByteString
  deriving newtype (Eq, Show)
