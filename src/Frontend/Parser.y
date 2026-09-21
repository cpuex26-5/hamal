{
module Frontend.Parser (parse) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe (fromJust)
import Data.Monoid (First (..))

import qualified Frontend.Lexer as L
}

%name parse
%tokentype { L.SpannedToken }
%error { parseError }
%monad { L.Alex } { >>= } { pure }
%lexer { lexer } { L.SpannedToken L.TEof _ }

%%

empty : {}

{
parseError :: L.SpannedToken -> L.Alex a
parseError _ = do
  (L.AlexPn _ line column, _, _, _) <- L.alexGetInput
  L.alexError $ "Parse error at line " <> show line <> ", column " <> show column

lexer :: (L.SpannedToken -> L.Alex a) -> L.Alex a
lexer = (=<< L.alexMonadScan)
}
