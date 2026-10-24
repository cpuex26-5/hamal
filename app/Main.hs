module Main (main) where

import Backend.Riscv.RegAlloc (regAlloc)
import Backend.Riscv.Virtual (genVirtual)
import Data.ByteString.Lazy.Char8 qualified as BS
import Frontend.Lexer (runAlex)
import Frontend.Parser (parse)
import Id (runFresh)
import Middleend.Closure (closureConvert)
import Middleend.KNormal (kNormal)
import Middleend.Opt.Alpha (alphaConvert)
import Middleend.Opt.Assoc (assocLet)
import Middleend.Opt.Beta (betaConvert)
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
      let assoc = assocLet <$> beta
      let closured = closureConvert <$> assoc
      let virtual = genVirtual =<< closured
      let regalloc = regAlloc <$> virtual
      putStrLn "---------- syntax ----------"
      dump syntax
      putStrLn "---------- knorm ----------"
      dump . runFresh $ knorm
      putStrLn "---------- alpha ----------"
      dump . runFresh $ alpha
      putStrLn "---------- beta ----------"
      dump . runFresh $ beta
      putStrLn "---------- assoc ----------"
      dump . runFresh $ assoc
      putStrLn "---------- closure ----------"
      dump . runFresh $ closured
      putStrLn "---------- virtual ----------"
      dump . runFresh $ virtual
      putStrLn "---------- regAlloc ----------"
      dump . runFresh $ regalloc
  where
    dump :: (Show a) => a -> IO ()
    dump = P.pPrintOpt P.NoCheckColorTty P.defaultOutputOptionsDarkBg {P.outputOptionsCompact = True}
