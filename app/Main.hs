module Main (main) where

import Data.ByteString.Lazy.Char8 qualified as BS
import Frontend.Lexer (SpannedToken (..), Token (..), alexMonadScan, runAlex)

scanMany :: BS.ByteString -> Either String [SpannedToken]
scanMany input = runAlex input go
  where
    go = do
      st <- alexMonadScan
      if st.stToken == TEof
        then pure [st]
        else (st :) <$> go

main :: IO ()
main = print . scanMany =<< BS.getContents
