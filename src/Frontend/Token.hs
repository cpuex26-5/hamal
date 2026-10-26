module Frontend.Token (Token (..), symbol, literal, identOf, intOf, floatOf, boolOf) where

import Control.Monad (guard)
import Data.ByteString (toStrict)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.ByteString.Short (toShort)
import Frontend.SrcSpan (Located (..), SrcSpan)
import Name (Id, internal)
import Prelude hiding (span)

data Token
  = TIdent BS.ByteString
  | TInt Int
  | TFloat Float
  | TBool Bool
  | TLet
  | TRec
  | TIn
  | TIf
  | TThen
  | TElse
  | TNot
  | TArrayCreate
  | TPlus
  | TMinus
  | TPlusDot
  | TMinusDot
  | TTimesDot
  | TDivideDot
  | TEq
  | TNeq
  | TLt
  | TLe
  | TGt
  | TGe
  | TLPar
  | TRPar
  | TComma
  | TSemicolon
  | TDot
  | TLeftArrow
  | TEof
  deriving stock (Eq, Show)

symbol :: Token -> Located Token -> Maybe SrcSpan
symbol token Located {value, span} = span <$ guard (value == token)

literal :: (Token -> Maybe a) -> Located Token -> Maybe (Located a)
literal = traverse

identOf :: Token -> Maybe Id
identOf (TIdent ident) = Just . internal . toShort . toStrict $ ident
identOf _ = Nothing

intOf :: Token -> Maybe Int
intOf (TInt int) = Just int
intOf _ = Nothing

floatOf :: Token -> Maybe Float
floatOf (TFloat float) = Just float
floatOf _ = Nothing

boolOf :: Token -> Maybe Bool
boolOf (TBool bool) = Just bool
boolOf _ = Nothing
