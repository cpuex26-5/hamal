{
module Frontend.Parser (parse) where

import Frontend.Lexer
import Frontend.Token
import Span
import Syntax
}

%name parse
%tokentype { Spanned Token }
%error { parseError }
%monad { Alex } { >>= } { pure }
%lexer { lexer } { Spanned { value = TEof } }
%expect 0

%token
  ident       { (literal identOf -> Just $$) }
  bool        { (literal boolOf -> Just $$) }
  int         { (literal intOf -> Just $$) }
  float       { (literal floatOf -> Just $$) }
  let         { (symbol TLet -> Just $$) }
  rec         { (symbol TRec -> Just $$) }
  in          { (symbol TIn -> Just $$) }
  if          { (symbol TIf -> Just $$) }
  then        { (symbol TThen -> Just $$) }
  else        { (symbol TElse -> Just $$) }
  not         { (symbol TNot -> Just $$) }
  arraycreate { (symbol TArrayCreate -> Just $$) }
  '+'         { (symbol TPlus -> Just $$) }
  '-'         { (symbol TMinus -> Just $$) }
  '+.'        { (symbol TPlusDot -> Just $$) }
  '-.'        { (symbol TMinusDot -> Just $$) }
  '*.'        { (symbol TTimesDot -> Just $$) }
  '/.'        { (symbol TDivideDot -> Just $$) }
  '='         { (symbol TEq -> Just $$) }
  '<>'        { (symbol TNeq -> Just $$) }
  '<'         { (symbol TLt -> Just $$) }
  '<='        { (symbol TLe -> Just $$) }
  '>'         { (symbol TGt -> Just $$) }
  '>='        { (symbol TGe -> Just $$) }
  '('         { (symbol TLPar -> Just $$) }
  ')'         { (symbol TRPar -> Just $$) }
  ','         { (symbol TComma -> Just $$) }
  ';'         { (symbol TSemicolon -> Just $$) }
  '.'         { (symbol TDot -> Just $$) }
  '->'        { (symbol TRightArrow -> Just $$) }
  '<-'        { (symbol TLeftArrow -> Just $$) }
%%

simple_expr :: { Expr Span }
  : '(' expr ')'  { $2 { ann = $1 <> $3 } }
  | '(' ')'  { Expr ($1 <> $2) Unit }
  | bool  { Expr $1.span (Bool $1.value) }
  | int  { Expr $1.span (Int $1.value) }
  | float  { Expr $1.span (Float $1.value) }
  | ident  { Expr $1.span (Var (Name $1.value)) }
  | simple_expr '.' '(' expr ')'  { Expr ($1.ann <> $5) (Get $1 $4) }

expr :: { Expr Span }
  : simple_expr { $1 }

{
parseError :: Spanned Token -> Alex a
parseError _ = do
  (AlexPn _ line column, _, _, _) <- alexGetInput
  alexError $ "Parse error at line " <> show line <> ", column " <> show column

lexer :: (Spanned Token -> Alex a) -> Alex a
lexer = (=<< alexMonadScan)
}
