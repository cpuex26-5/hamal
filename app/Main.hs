module Main (main) where

import Data.ByteString.Lazy.Char8 qualified as BS
import Frontend.Alpha (alphaConvert)
import Frontend.Beta (betaConvert)
import Frontend.Id (runFresh)
import Frontend.KNormal (kNormal)
import Frontend.Lexer (runAlex)
import Frontend.Parser (parse)
import Text.Pretty.Simple qualified as P

main :: IO ()
main = do
  input <- BS.getContents
  case runAlex input parse of
    Left err -> putStrLn err
    Right syntax -> do
      let knorm = kNormal syntax
      let alpha = alphaConvert knorm
      let beta = betaConvert <$> alpha
      putStrLn "---------- syntax ----------"
      dump syntax
      putStrLn "---------- knorm ----------"
      dump . runFresh $ knorm
      putStrLn "---------- alpha ----------"
      dump . runFresh $ alpha
      putStrLn "---------- beta ----------"
      dump . runFresh $ beta
  where
    dump :: (Show a) => a -> IO ()
    dump = P.pPrintOpt P.NoCheckColorTty P.defaultOutputOptionsDarkBg {P.outputOptionsCompact = True}
