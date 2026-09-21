{
module Frontend.Parser (parse) where

import Frontend.Lexer
import Syntax
}

%name parse
%tokentype { SpannedToken }
%error { parseError }
%monad { Alex } { >>= } { pure }
%lexer { lexer } { SpannedToken TEof _ }


%token
  ident       { SpannedToken (TIdent $$) _ }
  bool        { SpannedToken (TBool $$) _ }
  int         { SpannedToken (TInt $$) _ }
  float       { SpannedToken (TFloat $$) _ }
  let         { SpannedToken TLet _ }
  rec         { SpannedToken TRec _ }
  in          { SpannedToken TIn _ }
  if          { SpannedToken TIf _ }
  then        { SpannedToken TThen _ }
  else        { SpannedToken TElse _ }
  not         { SpannedToken TNot _ }
  arraycreate { SpannedToken TArrayCreate _ }
  '+'         { SpannedToken TPlus _ }
  '-'         { SpannedToken TMinus _ }
  '+.'        { SpannedToken TPlusDot _ }
  '-.'        { SpannedToken TMinusDot _ }
  '*.'        { SpannedToken TTimesDot _ }
  '/.'        { SpannedToken TDivideDot _ }
  '='         { SpannedToken TEq _ }
  '<>'        { SpannedToken TNeq _ }
  '<'         { SpannedToken TLt _ }
  '<='        { SpannedToken TLe _ }
  '>'         { SpannedToken TGt _ }
  '>='        { SpannedToken TGe _ }
  '('         { SpannedToken TLPar _ }
  ')'         { SpannedToken TRPar _ }
  ','         { SpannedToken TComma _ }
  ';'         { SpannedToken TSemicolon _ }
  '.'         { SpannedToken TDot _ }
  '->'        { SpannedToken TRightArrow _ }
  '<-'        { SpannedToken TLeftArrow _ }
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
parseError :: SpannedToken -> Alex a
parseError _ = do
  (AlexPn _ line column, _, _, _) <- alexGetInput
  alexError $ "Parse error at line " <> show line <> ", column " <> show column

lexer :: (SpannedToken -> Alex a) -> Alex a
lexer = (=<< alexMonadScan)
}
