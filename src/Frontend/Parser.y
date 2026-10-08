{
module Frontend.Parser (parse) where

import Frontend.Lexer
import Span
import Syntax
}

%name parse
%tokentype { Spanned Token }
%error { parseError }
%monad { Alex } { >>= } { pure }
%lexer { lexer } { Spanned { value = TEof } }

%token
  ident       { Spanned { value = TIdent $$ } }
  bool        { Spanned { value = TBool $$ } }
  int         { Spanned { value = TInt $$ } }
  float       { Spanned { value = TFloat $$ } }
  let         { Spanned { value = TLet } }
  rec         { Spanned { value = TRec } }
  in          { Spanned { value = TIn } }
  if          { Spanned { value = TIf } }
  then        { Spanned { value = TThen } }
  else        { Spanned { value = TElse } }
  not         { Spanned { value = TNot } }
  arraycreate { Spanned { value = TArrayCreate } }
  '+'         { Spanned { value = TPlus } }
  '-'         { Spanned { value = TMinus } }
  '+.'        { Spanned { value = TPlusDot } }
  '-.'        { Spanned { value = TMinusDot } }
  '*.'        { Spanned { value = TTimesDot } }
  '/.'        { Spanned { value = TDivideDot } }
  '='         { Spanned { value = TEq } }
  '<>'        { Spanned { value = TNeq } }
  '<'         { Spanned { value = TLt } }
  '<='        { Spanned { value = TLe } }
  '>'         { Spanned { value = TGt } }
  '>='        { Spanned { value = TGe } }
  '('         { Spanned { value = TLPar } }
  ')'         { Spanned { value = TRPar } }
  ','         { Spanned { value = TComma } }
  ';'         { Spanned { value = TSemicolon } }
  '.'         { Spanned { value = TDot } }
  '->'        { Spanned { value = TRightArrow } }
  '<-'        { Spanned { value = TLeftArrow } }
%%

simple_expr :: { Expr }
  : '(' expr ')'  { $2 }
  | '(' ')'  { Unit }
  | bool  { Bool $1 }
  | int  { Int $1 }
  | float  { Float $1 }
  | ident  { Var (Name $1) }
  | simple_expr '.' '(' expr ')'  { Get $1 $4 }

expr :: { Expr }
  : simple_expr { $1 }

{
parseError :: Spanned Token -> Alex a
parseError _ = do
  (AlexPn _ line column, _, _, _) <- alexGetInput
  alexError $ "Parse error at line " <> show line <> ", column " <> show column

lexer :: (Spanned Token -> Alex a) -> Alex a
lexer = (=<< alexMonadScan)
}
