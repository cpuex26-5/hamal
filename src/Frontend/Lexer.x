{

{-# LANGUAGE FieldSelectors #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies -Wno-operator-whitespace -Wno-term-variable-capture #-}

module Frontend.Lexer (
  Alex,
  AlexPosn (..),
  alexGetInput,
  alexError,
  runAlex,
  alexMonadScan,
) where

import Control.Monad (void, when)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Int (Int64)
import Frontend.Token (Token (..))
import Numeric (readFloat)
import Span (Posn(..), Span(..), Spanned(..))
}

%wrapper "monadUserState-bytestring"

$digit = [0-9]
$alpha = [a-zA-Z]

@id = ($alpha | \_) ($alpha | $digit | \_ | \' | \?)*

tokens :-

<0> $white+                                     ;

<0>       "(*"                                  { nestComment `andBegin` comment }
<0>       "*)"                                  { \_ _ -> alexError "Error: unexpected closing comment" }
<comment> "(*"                                  { nestComment }
<comment> "*)"                                  { unnestComment }
<comment> .                                     ;
<comment> \n                                    ;

<0> let                                         { tok TLet }
<0> rec                                         { tok TRec }
<0> in                                          { tok TIn }
<0> if                                          { tok TIf }
<0> then                                        { tok TThen }
<0> else                                        { tok TElse }
<0> not                                         { tok TNot }
<0> true                                        { tok (TBool True) }
<0> false                                       { tok (TBool False) }
<0> ("Array.create" | "Array.make")             { tok TArrayCreate }

<0> "+"                                         { tok TPlus }
<0> "-"                                         { tok TMinus }
<0> "+."                                        { tok TPlusDot }
<0> "-."                                        { tok TMinusDot }
<0> "*."                                        { tok TTimesDot }
<0> "/."                                        { tok TDivideDot }

<0> "="                                         { tok TEq }
<0> "<>"                                        { tok TNeq }
<0> "<"                                         { tok TLt }
<0> "<="                                        { tok TLe }
<0> ">"                                         { tok TGt }
<0> ">="                                        { tok TGe }

<0> "("                                         { tok TLPar }
<0> ")"                                         { tok TRPar }

<0> ","                                         { tok TComma }
<0> ";"                                         { tok TSemicolon }
<0> "."                                         { tok TDot }

<0> "->"                                        { tok TRightArrow }
<0> "<-"                                        { tok TLeftArrow }

<0> @id                                         { tokIdent }
<0> $digit+                                     { tokInt }
<0> $digit+ "." $digit* ([eE] [\+\-]? $digit+)? { tokFloat }

{
data AlexUserState = AlexUserState {nestLevel :: Int}

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState {nestLevel = 0}

modifyNestLevel :: (Int -> Int) -> Alex Int
modifyNestLevel f = do
  ust <- alexGetUserState
  let level = f ust.nestLevel
  alexSetUserState ust {nestLevel = level}
  pure level

nestComment, unnestComment :: AlexAction (Spanned Token)
nestComment input len = do
  void $ modifyNestLevel (+ 1)
  skip input len
unnestComment input len = do
  level <- modifyNestLevel (subtract 1)
  when (level == 0) $ alexSetStartCode 0
  skip input len

alexEOF :: Alex (Spanned Token)
alexEOF = do
  startCode <- alexGetStartCode
  when (startCode == comment) $ alexError "Error: unclosed comment"
  (pos, _, _, _) <- alexGetInput
  pure $ Spanned { value = TEof, span = Span { start = posn pos, stop = posn pos } }

posn :: AlexPosn -> Posn
posn (AlexPn offset line column) = Posn {offset, line, column}

mkSpan :: AlexInput -> Int64 -> Span
mkSpan (start, _, str, _) len = Span {start = posn start, stop = posn stop}
  where
    stop = BS.foldl' alexMove start $ BS.take len str

tok :: Token -> AlexAction (Spanned Token)
tok value input len = pure $ Spanned {value, span = mkSpan input len}

tokIdent :: AlexAction (Spanned Token)
tokIdent input@(_, _, str, _) len =
  pure
    $ Spanned
      { value = TIdent $ BS.take len str
      , span = mkSpan input len
      }

tokInt :: AlexAction (Spanned Token)
tokInt input@(_, _, str, _) len = do
  let digits = BS.take len str
  int <- case BS.readInt digits of
    Just (int, rest) | BS.null rest -> pure int
    _ -> alexError $ "Error: malformed or out-of-range integer literal " <> BS.unpack digits
  pure
    $ Spanned
        { value = TInt int
        , span = mkSpan input len
        }

tokFloat :: AlexAction (Spanned Token)
tokFloat input@(_, _, str, _) len = do
  let lexeme = BS.unpack $ BS.take len str
  float <- case readFloat lexeme of
    [(float, rest)] | rest `elem` ["", "."] -> pure float
    _ -> alexError $ "Error: malformed float literal " <> lexeme
  pure $
    Spanned
      { value = TFloat float
      , span = mkSpan input len
      }
}
