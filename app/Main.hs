module Main (main) where

import Data.ByteString.Lazy.Char8 qualified as BS
import Frontend.Alpha (alphaConvert)
import Frontend.Id (runFresh)
import Frontend.KNormal (kNormal, prettyKNormal)
import Frontend.Lexer (runAlex)
import Frontend.Parser (parse)

main :: IO ()
main = do
  input <- BS.getContents
  case runAlex input parse of
    Left err -> putStrLn err
    Right syntax -> do
      putStrLn "---------- syntax ----------"
      print syntax
      let knorm = kNormal syntax
      let alpha = alphaConvert knorm
      putStrLn "---------- knorm ----------"
      putStrLn . BS.unpack . prettyKNormal . runFresh $ knorm
      putStrLn "---------- alpha ----------"
      putStrLn . BS.unpack . prettyKNormal . runFresh $ alpha
