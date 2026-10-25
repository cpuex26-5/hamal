module Main (main) where

import Control.Applicative ((<**>))
import Control.Monad (when)
import Data.ByteString.Builder (hPutBuilder)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Set qualified as S
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.Error.Static (runErrorNoCallStack)
import GHC.IO.StdHandles (stderr, stdout)
import Hamal (CompileError (ParseError), Dump (..), Phase, compile, phase)
import Options (Options (..), optionsP)
import Options.Applicative (execParser, fullDesc, helper, info, progDesc)
import System.Exit (exitFailure)
import System.IO (hPutStrLn)
import Text.Pretty.Simple qualified as P

runDump :: (IOE :> es) => S.Set Phase -> Eff (Dump : es) a -> Eff es a
runDump dumps = interpret_ \(Dump s x) ->
  when (phase s `S.member` dumps) $
    P.pHPrintOpt
      P.NoCheckColorTty
      P.defaultOutputOptionsDarkBg {P.outputOptionsCompact = True}
      stderr
      x

main :: IO ()
main = do
  opts <- execParser $ info (optionsP <**> helper) (fullDesc <> progDesc "Hamal: A MinCaml compiler writtern in Haskell.")
  input <- BS.getContents
  m <- runEff . runErrorNoCallStack @CompileError . runDump opts.dumps . compile $ input
  case m of
    Left (ParseError msg) -> hPutStrLn stderr msg >> exitFailure
    Right bb -> hPutBuilder stdout bb
