{
module Frontend.Parser (parse) where

import Control.Comonad (extract)
import Control.Comonad.Cofree
import Data.Foldable1
import Data.List.NonEmpty
import Frontend.Lexer
import Frontend.SrcSpan
import Frontend.Syntax
import Frontend.Token
import Name
import PrimOp
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
  '*'         { (symbol TTimes -> Just $$) }
  '/'         { (symbol TDivide -> Just $$) }
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
%left '*' '/' '*.' '/.'
%right prec_unary_minus
%left prec_app
%left '.'

%%

expr :: { Parsed }
  : simple_expr { $1 }
  | not expr %prec prec_app { $1 <> extract $2 :< Not $2 }
  | '-' expr %prec prec_unary_minus { case $2 of
                                        ann :< Float float -> $1 <> ann :< Float (-float)
                                        ann :< _           -> $1 <> ann :< PrimOp (Neg $2) }
  | '-.' expr %prec prec_unary_minus { $1 <> extract $2 :< PrimOp (FNeg $2) }
  | expr '+'  expr { extract $1 <> extract $3 :< PrimOp (Add $1 $3) }
  | expr '-'  expr { extract $1 <> extract $3 :< PrimOp (Sub $1 $3) }
  | expr '*'  expr { extract $1 <> extract $3 :< PrimOp (Mul $1 $3) }
  | expr '/'  expr { extract $1 <> extract $3 :< PrimOp (Div $1 $3) }
  | expr '+.' expr { extract $1 <> extract $3 :< PrimOp (FAdd $1 $3) }
  | expr '-.' expr { extract $1 <> extract $3 :< PrimOp (FSub $1 $3) }
  | expr '*.' expr { extract $1 <> extract $3 :< PrimOp (FMul $1 $3) }
  | expr '/.' expr { extract $1 <> extract $3 :< PrimOp (FDiv $1 $3) }
  | expr '='  expr { extract $1 <> extract $3 :< Cmp Eq $1 $3 }
  | expr '<=' expr { extract $1 <> extract $3 :< Cmp Le $1 $3 }
  | expr '>=' expr { extract $1 <> extract $3 :< Cmp Le $3 $1 }
  | expr '<>' expr { extract $1 <> extract $3 :< Not (extract $1 <> extract $3 :< Cmp Eq $1 $3) }
  | expr '<'  expr { extract $1 <> extract $3 :< Not (extract $1 <> extract $3 :< Cmp Le $3 $1) }
  | expr '>'  expr { extract $1 <> extract $3 :< Not (extract $1 <> extract $3 :< Cmp Le $1 $3) }
  | if expr then expr else expr %prec prec_if { $1 <> extract $6 :< If $2 $4 $6 }
  | let ident '=' expr in expr %prec prec_let { $1 <> extract $6 :< Let $2.value $4 $6 }
  | let rec funbind in expr    %prec prec_let { $1 <> extract $5 :< LetRec $3 $5 }
  | simple_expr parameters     %prec prec_app { foldMap1 extract ($1 :| $2) :< App $1 $2 }
  | elems %prec prec_tuple { foldMap1 extract $1 :< PrimOp (Tuple (toList $1)) }
  | let '(' pattern ')' '=' expr in expr { $1 <> extract $8 :< LetTuple $3 $6 $8 }
  | simple_expr '.' '(' expr ')' '<-' expr { extract $1 <> extract $7 :< PrimOp (Put $1 $4 $7) }
  | expr ';' expr { extract $1 <> extract $3 :< Let (Symbol "_") $1 $3 }
  | arraycreate simple_expr simple_expr %prec prec_app { $1 <> extract $3 :< Array $2 $3 }


simple_expr :: { Parsed }
  : '(' expr ')'                  { $2 }
  | '(' ')'                       { $1 <> $2 :< Unit }
  | bool                          { $1.span :< Bool $1.value }
  | int                           { $1.span :< Int $1.value }
  | float                         { $1.span :< Float $1.value }
  | ident                         { $1.span :< Var $1.value }
  | simple_expr '.' '(' expr ')'  { extract $1 <> $5 :< PrimOp (Get $1 $4) }

funbind :: { FunBind Symbol Parsed }
  : ident arguments '=' expr { FunBind $1.value $2 $4 }

arguments :: { [Symbol] }
  : ident arguments { $1.value : $2 }
  | ident           { [$1.value] }

parameters :: { [Parsed] }
  : parameters simple_expr %prec prec_app { $1 <> [$2] }
  | simple_expr            %prec prec_app { [$1] }

elems :: { NonEmpty (Parsed) }
  : elems ',' expr { $1 <> pure $3 }
  | expr  ',' expr { $1 :| pure $3 }

pattern :: { [Symbol] }
  : pattern ',' ident { $1 <> [$3.value] }
  | ident   ',' ident { [$1.value, $3.value] }

{
parseError :: Located Token -> Alex a
parseError _ = do
  (AlexPn _ line column, _, _, _) <- alexGetInput
  alexError $ "Parse error at line " <> show line <> ", column " <> show column

lexer :: (Located Token -> Alex a) -> Alex a
lexer = (=<< alexMonadScan)
}
