module Options (Options (..), optionsP) where

import Control.Applicative (asum, many)
import Data.Set qualified as S
import Hamal (Phase, phaseName)
import Options.Applicative (Parser, flag', help, long)

newtype Options = Options {dumps :: S.Set Phase}

optionsP :: Parser Options
optionsP = Options <$> dumpsP

dumpsP :: Parser (S.Set Phase)
dumpsP = S.fromList . concat <$> many (asum (allP : (phaseP <$> [minBound ..])))
  where
    phaseP p = flag' [p] $ long ("dump-" <> phaseName p) <> help ("Dump the IR after" <> phaseName p)
    allP = flag' [minBound ..] (long "dump-all" <> help "Dump the IR after every phase")
