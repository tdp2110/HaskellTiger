{
module Parser where

import Prelude hiding (GT, LT, EQ, init)
import Lexer hiding (Pos)
import AbSyn
}

%name calc
%tokentype { Lexeme }
%error     { parseError }
%monad{ Either String }{ >>= }{ return }

%token
  id        { Lexeme _ (ID $$) _ }
  int       { Lexeme _ (INT $$) _ }
  string    { Lexeme _ (STRING $$) _ }
  ','       { Lexeme _ COMMA _ }
  ':'       { Lexeme _ COLON _ }
  ';'       { Lexeme _ SEMICOLON _ }
  '('       { Lexeme _ LPAREN _ }
  ')'       { Lexeme _ RPAREN _ }
  '['       { Lexeme _ LBRACK _ }
  ']'       { Lexeme _ RBRACK _ }
  '{'       { Lexeme _ LBRACE _ }
  '}'       { Lexeme _ RBRACE _ }
  '.'       { Lexeme _ DOT _ }
  '+'       { Lexeme _ PLUS _ }
  '-'       { Lexeme _ MINUS _ }
  '*'       { Lexeme _ TIMES _ }
  '/'       { Lexeme _ DIVIDE _ }
  nil       { Lexeme _ NIL _ }
  '>='      { Lexeme _ GE _ }
  '<='      { Lexeme _ LE _ }
  '='       { Lexeme _ EQ _ }
  '<>'      { Lexeme _ NEQ _ }
  '<'       { Lexeme _ LT _ }
  '>'       { Lexeme _ GT _ }
  '|'       { Lexeme _ OR _ }
  '&'       { Lexeme _ AND _ }
  while     { Lexeme _ WHILE _ }
  do        { Lexeme _ DO _ }
  if        { Lexeme _ IF _ }
  then      { Lexeme _ THEN _ }
  else      { Lexeme _ ELSE _ }
  for       { Lexeme _ FOR _ }
  ':='      { Lexeme _ ASSIGN _ }
  to        { Lexeme _ DO _ }
  break     { Lexeme _ BREAK _ }
  type      { Lexeme _ TYPE _ }
  var       { Lexeme _ VAR _ }
  function  { Lexeme _ FUNCTION _ }
  --primitive { Lexeme _ FUNCTION _ }
  let       { Lexeme _ LET _ }
  in        { Lexeme _ IN _ }
  end       { Lexeme _ END _ }
  of        { Lexeme _ OF _ }

%right of
%nonassoc else
%nonassoc do
%nonassoc ':='
%left '|'
%left '&'
%nonassoc '>=' '<=' '=' '<>' '<' '>'
%left '+' '-'
%left '*' '/'
%left UMINUS

%%

program :: { Exp }
  : exp { $1 }

exp :: { Exp }
  : nil                            { NilExp }
  | int                            { IntExp $1 }
  | string                         { StringExp $1 }
  | lvalue                         { VarExp $1 }
  | typeId '{' recordFields '}'    { RecordExp{fields=$3, typ=$1} }
  | typeId '[' exp ']' of exp      { ArrayExp{typ=$1, size=$3, init=$6} }
  | id '(' callArgs ')'            { CallExp{func=$1, args=$3} }
--| lvalue '.' id '(' callArgs ')' { Method ... }
  | exp '+' exp                    { OpExp $1 PlusOp $3 }
  | exp '-' exp                    { OpExp $1 MinusOp $3 }
  | exp '*' exp                    { OpExp $1 TimesOp $3 }
  | exp '/' exp                    { OpExp $1 DivideOp $3 }
  | exp '>=' exp                   { OpExp $1 GeOp $3 }
  | exp '<=' exp                   { OpExp $1 LeOp $3 }
  | exp '=' exp                    { OpExp $1 EqOp $3 }
  | exp '<>' exp                   { OpExp $1 NeqOp $3 }
  | exp '<' exp                    { OpExp $1 LtOp $3 }
  | exp '>' exp                    { OpExp $1 GtOp $3 }
  | exp '|' exp                    { IfExp{test=$1, then'=(IntExp 1), else'=(Just $3)} }
  | exp '&' exp                    { IfExp{test=$1, then'=$3, else'=Just (IntExp 0)} }
  | '-' exp %prec UMINUS           { OpExp (IntExp 0) MinusOp $2 }
  | '(' exps ')'                   { $2 }
  | if exp then exp elseTail       { IfExp{test=$2, then'=$4, else'=$5} }
  | while exp do exp               { WhileExp{test=$2, body=$4} }
  | for id ':=' exp to exp do exp  { ForExp{forVar=$2, lo=$4, hi=$6, body=$8} }
  | break                          { BreakExp{} }
  | let decs in exps end           { LetExp{decs=$2, body=$4} }

lvalue :: { Var }
  : id          { SimpleVar $1 0 }
  | lvaluePrime { $1 }

lvaluePrime :: { Var }
  : lvaluePrime '.' id      { FieldVar $1 $3 0 }
  | id '.' id               { FieldVar (SimpleVar $1 0) $3 0 }
  | lvaluePrime '[' exp ']' { SubscriptVar $1 $3 0 }
  | id '[' exp ']'          { SubscriptVar (SimpleVar $1 0) $3 0 }

recordFields :: { [(Symbol, Exp, Pos)] }
  : id '=' exp recordFieldsTail { ($1, $3, 0) : $4 }
  | {- empty -}                 { [] }

recordFieldsTail :: { [(Symbol, Exp, Pos)] }
  : ',' id '=' exp recordFieldsTail { ($2, $4, 0) : $5 }
  | {- empty -}                     { [] }

decs :: { [Dec] }
  : dec decs { $1 : $2 }
  | {- empty -} { [] }

dec :: { Dec }
  : type id '=' ty                          { TypeDec{name=$2, ty=$4} }
--| class id [ extends type-id ]            { classfields }
  | vardec                                  { $1 }
  | function id '(' tyFields ')' optTypeId  { funDec $2 $4 $6 }
--| primitive id ( tyfields ) [ : type-id ]
--| import string

{-
    classfields ::= { classfield }
    # Class fields.
    classfield ::=
      # Attribute declaration.
        vardec
      # Method declaration.
        | method id ( tyfields ) [ : type-id ] = exp
-}

vardec :: { Dec }
  : var id optTypeId ':=' exp { VarDec{name=$2, varDecTyp=$3, decInit=$5} }

optTypeId :: { Maybe (Symbol, Pos) }
  : ':' typeId  { Just ($2, 0) }
  | {- empty -} { Nothing }

ty :: { Ty }
  : typeId           { NameTy ($1, 0) }
  | '{' tyFields '}' { RecordTy $2 }

tyFields :: { [Field] }
  : id ':' typeId tyFieldTail { field $1 $3 : $4 }
  | {- empty -}               { [] }

tyFieldTail :: { [Field] }
  : ',' id ':' typeId tyFieldTail { field $2 $4 : $5 }
  | {- empty -}                   { [] }

typeId :: { Symbol }
  : id { $1 }

exps :: { Exp }
  : exp expsTail { seqConcat $1 $2 }
  | {- empty -}  { SeqExp [] }

expsTail :: { Exp }
  : ';' exp expsTail { seqConcat $2 $3 }
  | {- empty -}      { SeqExp [] }

elseTail :: { Maybe Exp }
  : else exp    { Just $2 }
  | {- empty -} { Nothing }

callArgs :: { [Exp] }
  : exp callArgs { $1 : $2 }
  | {- empty -}  { [] }

{
seqConcat :: Exp -> Exp -> Exp
seqConcat exp (SeqExp (exp':exps)) = SeqExp ((exp, 0):(exp':exps))
seqConcat exp (SeqExp []) = SeqExp [(exp, 0)]

field :: Symbol -> Symbol -> Field
field fieldName' fieldTyp' = Field{fieldName=fieldName', fieldTyp=fieldTyp'}

funDec :: Symbol -> [Field] -> Maybe(Symbol, Pos) -> Dec
funDec name fields maybeTy = FunctionDec [FunDec{fundecName=name, params=fields, result=maybeTy}]

parseError :: [Lexeme] -> a
parseError [] = error "Parse Error at EOF"
parseError (x:xs) = error ("Parse Error at token " ++ show x)

parse :: String -> Either String Exp
parse input = scanner input >>= calc

}
