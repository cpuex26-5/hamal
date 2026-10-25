module Main (main) where

import Backend.Riscv.Emit (emit)
import Backend.Riscv.RegAlloc (regAlloc)
import Backend.Riscv.Virtual (genVirtual)
import Data.ByteString.Builder (hPutBuilder)
import Data.ByteString.Lazy.Char8 qualified as BS
import Effectful (runEff)
import Frontend.Lexer (runAlex)
import Frontend.Parser (parse)
import GHC.IO.StdHandles (stdout)
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
      let alpha = alphaConvert =<< knorm
      let beta = betaConvert <$> alpha
      let assoc = assocLet <$> beta
      let closured = closureConvert <$> assoc
      let virtual = genVirtual =<< closured
      let regalloc = regAlloc <$> virtual
      let asm = emit =<< regalloc
      putStrLn "---------- syntax ----------"
      dump syntax
      putStrLn "---------- knorm ----------"
      dump =<< runEff (runFresh knorm)
      putStrLn "---------- alpha ----------"
      dump =<< runEff (runFresh alpha)
      putStrLn "---------- beta ----------"
      dump =<< runEff (runFresh beta)
      putStrLn "---------- assoc ----------"
      dump =<< runEff (runFresh assoc)
      putStrLn "---------- closure ----------"
      dump =<< runEff (runFresh closured)
      putStrLn "---------- virtual ----------"
      dump =<< runEff (runFresh virtual)
      putStrLn "---------- regAlloc ----------"
      dump =<< runEff (runFresh regalloc)
      putStrLn "---------- emit ----------"
      hPutBuilder stdout =<< (runEff . runFresh $ asm)
  where
    dump :: (Show a) => a -> IO ()
    dump = P.pPrintOpt P.NoCheckColorTty P.defaultOutputOptionsDarkBg {P.outputOptionsCompact = True}
