module Main (main) where

import Data.ByteString.Builder (hPutBuilder)
import Data.ByteString.Lazy.Char8 qualified as BS
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.Error.Static (runErrorNoCallStack)
import GHC.IO.StdHandles (stderr, stdout)
import Hamal (CompileError (ParseError), Dump (..), compile)
import System.Exit (exitFailure)
import System.IO (hPutStrLn)
import Text.Pretty.Simple qualified as P

runDump :: (IOE :> es) => Eff (Dump : es) a -> Eff es a
runDump = interpret_ \(Dump _ x) -> P.pHPrintOpt P.NoCheckColorTty P.defaultOutputOptionsDarkBg {P.outputOptionsCompact = True} stderr x

main :: IO ()
main = do
  input <- BS.getContents
  m <- runEff . runErrorNoCallStack @CompileError . runDump . compile $ input
  case m of
    Left (ParseError msg) -> hPutStrLn stderr msg >> exitFailure
    Right bb -> hPutBuilder stdout bb
