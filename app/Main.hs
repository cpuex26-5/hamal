module Main (main) where

import Data.ByteString.Lazy.Char8 qualified as BS
import Frontend.Lexer (runAlex)
import Frontend.Parser (parse)

main :: IO ()
main = do
  input <- BS.getContents
  print $ runAlex input parse
