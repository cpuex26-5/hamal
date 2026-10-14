module Main (main) where

import Data.ByteString.Lazy.Char8 qualified as BS
import Frontend.KNormal (kNormal)
import Frontend.Lexer (runAlex)
import Frontend.Parser (parse)

main :: IO ()
main = do
  input <- BS.getContents
  case runAlex input parse of
    Left err -> putStrLn err
    Right syntax -> do
      print syntax
      let knorm = kNormal syntax
      print knorm
