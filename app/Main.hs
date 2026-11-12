module Main (main) where

import Control.Applicative ((<**>))
import Control.Monad (when)
import Data.ByteString.Builder (hPutBuilder)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Set qualified as S
import Effectful (Eff, IOE, MonadIO (..), runEff, (:>))
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.Error.Static (runErrorNoCallStack)
import GHC.IO.StdHandles (stderr, stdout)
import Hamal (CompileError (..), Inspect (..), Stage (..), compile, phase, phaseName)
import Log (runLog)
import Options (Options (..), optionsP)
import Options.Applicative (execParser, fullDesc, helper, info, progDesc)
import Options.Applicative.Help (LayoutOptions (LayoutOptions), PageWidth (AvailablePerLine), Pretty (pretty), hardline, layoutPretty, renderIO)
import System.Exit (exitFailure)
import System.IO (hPrint, hPutStrLn)
import Text.Pretty.Simple qualified as P

runInspect :: (IOE :> es) => Options -> Eff (Inspect : es) a -> Eff es a
runInspect opts = interpret_ \(Inspect s x) ->
  when (phase s `S.member` opts.dumps) $ do
    let prologue = \case
          Nothing -> liftIO $ hPutStrLn stderr $ "==================== " <> phaseName (phase s) <> " ===================="
          Just i -> liftIO $ hPutStrLn stderr $ "==================== " <> phaseName (phase s) <> " #" <> show i <> " ===================="
    case s of
      SParsed -> prologue Nothing *> pSimple x
      STyped -> prologue Nothing *> pSimple x
      SKNormal -> prologue Nothing *> pCustom x
      STupleFlatten i -> prologue (Just i) *> pCustom x
      SBeta i -> prologue (Just i) *> pCustom x
      SAssoc i -> prologue (Just i) *> pCustom x
      SElim i -> prologue (Just i) *> pCustom x
      SOptimized -> prologue Nothing *> pCustom x
      SClosure -> prologue Nothing *> pSimple x
      SVirtual -> prologue Nothing *> pSimple x
      SRegAlloc -> prologue Nothing *> pSimple x
  where
    pSimple :: (MonadIO m, Show a) => a -> m ()
    pSimple = P.pHPrintOpt P.NoCheckColorTty P.defaultOutputOptionsDarkBg {P.outputOptionsCompact = True} stderr
    pCustom d = liftIO . renderIO stderr . layoutPretty (LayoutOptions (AvailablePerLine 100 1)) $ pretty d <> hardline

main :: IO ()
main = do
  opts <- execParser $ info (optionsP <**> helper) (fullDesc <> progDesc "Hamal: A MinCaml compiler written in Haskell.")
  input <- BS.getContents
  m <- runEff . runErrorNoCallStack @CompileError . runInspect opts . runLog opts.verbose . compile opts.iter $ input
  case m of
    Left (ParseError msg) -> hPutStrLn stderr msg >> exitFailure
    Left (TypeError typeError) -> hPrint stderr typeError >> exitFailure
    Right bb -> hPutBuilder stdout bb
