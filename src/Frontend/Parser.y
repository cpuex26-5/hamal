{
module Frontend.Parser (parse) where

import Data.Foldable1
import Data.List.NonEmpty
import Frontend.Lexer
import Frontend.SrcSpan
import Frontend.Syntax
import Frontend.Token
import Name
}

%name parse
%tokentype { Located Token }
%error { parseError }
%monad { Alex } { >>= } { pure }
%lexer { lexer } { Located { value = TEof } }
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
  '<-'        { (symbol TLeftArrow -> Just $$) }

%nonassoc in
%right prec_let
%right ';'
%right prec_if
%right '<-'
%nonassoc prec_tuple
%left ','
%left '=' '<>' '<' '>' '<=' '>='
%left '+' '-' '+.' '-.'
%left '*.' '/.'
%right prec_unary_minus
%left prec_app
%left '.'

%%

expr :: { Syntax SrcSpan }
  : simple_expr { $1 }
  | not expr %prec prec_app { Syntax ($1 <> $2.ann) (Not $2) }
  | '-' expr %prec prec_unary_minus { case $2.kind of
                                        Float float -> Syntax ($1 <> $2.ann) (Float (-float))
                                        _           -> Syntax ($1 <> $2.ann) (Neg $2) }
  | '-.' expr %prec prec_unary_minus { Syntax ($1 <> $2.ann) (FNeg $2) }
  | expr '+' expr { Syntax ($1.ann <> $3.ann) (Add $1 $3) }
  | expr '-' expr { Syntax ($1.ann <> $3.ann) (Sub $1 $3) }
  | expr '+.' expr { Syntax ($1.ann <> $3.ann) (FAdd $1 $3) }
  | expr '-.' expr { Syntax ($1.ann <> $3.ann) (FSub $1 $3) }
  | expr '*.' expr { Syntax ($1.ann <> $3.ann) (FMul $1 $3) }
  | expr '/.' expr { Syntax ($1.ann <> $3.ann) (FDiv $1 $3) }
  | expr '=' expr { Syntax ($1.ann <> $3.ann) (Eq $1 $3) }
  | expr '<>' expr { Syntax ($1.ann <> $3.ann) $ Not (Syntax ($1.ann <> $3.ann) (Eq $1 $3)) }
  | expr '<' expr { Syntax ($1.ann <> $3.ann) $ Not (Syntax ($1.ann <> $3.ann) (Le $3 $1)) }
  | expr '>' expr { Syntax ($1.ann <> $3.ann) $ Not (Syntax ($1.ann <> $3.ann) (Le $1 $3)) }
  | expr '<=' expr { Syntax ($1.ann <> $3.ann) (Le $1 $3) }
  | expr '>=' expr { Syntax ($1.ann <> $3.ann) (Le $3 $1) }
  | if expr then expr else expr %prec prec_if { Syntax ($1 <> $6.ann) $ If $2 $4 $6 }
  | let ident '=' expr in expr %prec prec_let { Syntax ($1 <> $6.ann) $ Let $2.value $4 $6 }
  | let rec funbind in expr %prec prec_let { Syntax ($1 <> $5.ann) $ LetRec $3 $5 }
  | simple_expr parameters %prec prec_app { Syntax (foldMap1 (.ann) ($1 :| $2)) $ App $1 $2 }
  | elems %prec prec_tuple { Syntax (foldMap1 (.ann) $1) $ Tuple (toList $1) }
  | let '(' pattern ')' '=' expr in expr { Syntax ($1 <> $8.ann) $ LetTuple $3 $6 $8 }
  | simple_expr '.' '(' expr ')' '<-' expr { Syntax ($1.ann <> $7.ann) $ Put $1 $4 $7 }
  | expr ';' expr { Syntax ($1.ann <> $3.ann) $ Let (internal "_") $1 $3 }
  | arraycreate simple_expr simple_expr %prec prec_app { Syntax ($1 <> $3.ann) $ Array $2 $3 }


simple_expr :: { Syntax SrcSpan }
  : '(' expr ')'  { $2 { ann = $1 <> $3 } }
  | '(' ')'  { Syntax ($1 <> $2) Unit }
  | bool  { Syntax $1.span (Bool $1.value) }
  | int  { Syntax $1.span (Int $1.value) }
  | float  { Syntax $1.span (Float $1.value) }
  | ident  { Syntax $1.span (Var $1.value) }
  | simple_expr '.' '(' expr ')'  { Syntax ($1.ann <> $5) (Get $1 $4) }

funbind :: { FunBind SrcSpan }
  : ident arguments '=' expr { FunBind $1.value $2 $4 }

arguments :: { [Id] }
  : ident arguments { $1.value : $2 }
  | ident { [$1.value] }

parameters :: { [Syntax SrcSpan] }
  : parameters simple_expr %prec prec_app { $1 <> [$2] }
  | simple_expr %prec prec_app { [$1] }

elems :: { NonEmpty (Syntax SrcSpan) }
  : elems ',' expr { $1 <> pure $3 }
  | expr ',' expr { $1 :| pure $3 }

pattern :: { [Id] }
  : pattern ',' ident { $1 <> [$3.value] }
  | ident ',' ident { [$1.value, $3.value] }

{
parseError :: Located Token -> Alex a
parseError _ = do
  (AlexPn _ line column, _, _, _) <- alexGetInput
  alexError $ "Parse error at line " <> show line <> ", column " <> show column

lexer :: (Located Token -> Alex a) -> Alex a
lexer = (=<< alexMonadScan)
}
