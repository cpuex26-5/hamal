module Buildable (Buildable (..)) where

import Data.ByteString.Builder (Builder)

class Buildable a where
  build :: a -> Builder
