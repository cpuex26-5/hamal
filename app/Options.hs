module Options (Options (..), optionsP) where

import Control.Applicative (asum, many)
import Data.Set qualified as S
import Hamal (Phase, phaseName)
import Options.Applicative (Parser, auto, flag', help, long, metavar, option, short, showDefault, switch, value)

data Options = Options
  { verbose :: Bool
  , iter :: Int
  , dumps :: S.Set Phase
  }

optionsP :: Parser Options
optionsP = Options <$> verboseP <*> iterP <*> dumpsP

verboseP :: Parser Bool
verboseP = switch $ short 'v' <> long "verbose" <> help "show log"

iterP :: Parser Int
iterP = option auto (long "iter" <> help "# of optimizing iterations" <> showDefault <> value 1000 <> metavar "n")

dumpsP :: Parser (S.Set Phase)
dumpsP = S.fromList . concat <$> many (asum (allP : (phaseP <$> [minBound ..])))
  where
    phaseP p = flag' [p] $ long ("dump-" <> phaseName p) <> help ("Dump the IR after " <> phaseName p)
    allP = flag' [minBound ..] (long "dump-all" <> help "Dump the IR after every phase")
