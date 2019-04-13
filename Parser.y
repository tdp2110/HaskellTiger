{
module Parser where

import Prelude hiding (GT, LT, EQ, init)
import qualified Lexer as L
import qualified AbSyn as A
}

%name calc
%tokentype { L.Lexeme }
%error     { parseError }
%monad{ Either String }{ >>= }{ return }

%token
  id        { L.Lexeme _ (L.ID $$) _ }
  int       { L.Lexeme _ (L.INT $$) _ }
  string    { L.Lexeme _ (L.STRING $$) _ }
  ','       { L.Lexeme _ L.COMMA _ }
  ':'       { L.Lexeme _ L.COLON _ }
  ';'       { L.Lexeme _ L.SEMICOLON _ }
  '('       { L.Lexeme _ L.LPAREN _ }
  ')'       { L.Lexeme _ L.RPAREN _ }
  '['       { L.Lexeme _ L.LBRACK _ }
  ']'       { L.Lexeme _ L.RBRACK _ }
  '{'       { L.Lexeme _ L.LBRACE _ }
  '}'       { L.Lexeme _ L.RBRACE _ }
  '.'       { L.Lexeme _ L.DOT _ }
  '+'       { L.Lexeme _ L.PLUS _ }
  '-'       { L.Lexeme _ L.MINUS _ }
  '*'       { L.Lexeme _ L.TIMES _ }
  '/'       { L.Lexeme _ L.DIVIDE _ }
  nil       { L.Lexeme _ L.NIL _ }
  '>='      { L.Lexeme _ L.GE _ }
  '<='      { L.Lexeme _ L.LE _ }
  '='       { L.Lexeme _ L.EQ _ }
  '<>'      { L.Lexeme _ L.NEQ _ }
  '<'       { L.Lexeme _ L.LT _ }
  '>'       { L.Lexeme _ L.GT _ }
  '|'       { L.Lexeme _ L.OR _ }
  '&'       { L.Lexeme _ L.AND _ }
  while     { L.Lexeme _ L.WHILE _ }
  do        { L.Lexeme _ L.DO _ }
  if        { L.Lexeme _ L.IF _ }
  then      { L.Lexeme _ L.THEN _ }
  else      { L.Lexeme _ L.ELSE _ }
  for       { L.Lexeme _ L.FOR _ }
  ':='      { L.Lexeme _ L.ASSIGN _ }
  to        { L.Lexeme _ L.DO _ }
  break     { L.Lexeme _ L.BREAK _ }
  type      { L.Lexeme _ L.TYPE _ }
  var       { L.Lexeme _ L.VAR _ }
  function  { L.Lexeme _ L.FUNCTION _ }
  --primitive { L.Lexeme _ L.FUNCTION _ }
  let       { L.Lexeme _ L.LET _ }
  in        { L.Lexeme _ L.IN _ }
  end       { L.Lexeme _ L.END _ }
  of        { L.Lexeme _ L.OF _ }

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

program :: { A.Exp }
  : exp { $1 }

exp :: { A.Exp }
  : nil                            { A.NilExp }
  | int                            { A.IntExp $1 }
  | string                         { A.StringExp $1 }
  | lvalue                         { A.VarExp $1 }
  | typeId '{' recordFields '}'    { A.RecordExp{A.fields=$3, A.typ=$1} }
  | typeId '[' exp ']' of exp      { A.ArrayExp{A.typ=$1, A.size=$3, A.init=$6} }
  | id '(' callArgs ')'            { A.CallExp{A.func=$1, A.args=$3} }
--| lvalue '.' id '(' callArgs ')' { A.Method ... }
  | exp '+' exp                    { A.OpExp $1 A.PlusOp $3 }
  | exp '-' exp                    { A.OpExp $1 A.MinusOp $3 }
  | exp '*' exp                    { A.OpExp $1 A.TimesOp $3 }
  | exp '/' exp                    { A.OpExp $1 A.DivideOp $3 }
  | exp '>=' exp                   { A.OpExp $1 A.GeOp $3 }
  | exp '<=' exp                   { A.OpExp $1 A.LeOp $3 }
  | exp '=' exp                    { A.OpExp $1 A.EqOp $3 }
  | exp '<>' exp                   { A.OpExp $1 A.NeqOp $3 }
  | exp '<' exp                    { A.OpExp $1 A.LtOp $3 }
  | exp '>' exp                    { A.OpExp $1 A.GtOp $3 }
  | exp '|' exp                    { A.IfExp{A.test=$1, A.then'=(A.IntExp 1), A.else'=(Just $3)} }
  | exp '&' exp                    { A.IfExp{A.test=$1, A.then'=$3, A.else'=Just (A.IntExp 0)} }
  | '-' exp %prec UMINUS           { A.OpExp (A.IntExp 0) A.MinusOp $2}
  | '(' exps ')'                   { $2 }
  | if exp then exp elseTail       { A.IfExp{A.test=$2, A.then'=$4, A.else'=$5} }
  | while exp do exp               { A.WhileExp{A.test=$2, A.body=$4} }
  | for id ':=' exp to exp do exp  { A.ForExp{A.forVar=$2, A.lo=$4, A.hi=$6, A.body=$8} }
  | break                          { A.BreakExp{} }
  | let decs in exps end           { A.LetExp{A.decs=$2, A.body=$4} }

lvalue :: { A.Var }
  : id          { A.SimpleVar $1 0 }
  | lvaluePrime { $1 }

lvaluePrime :: { A.Var }
  : lvaluePrime '.' id      { A.FieldVar $1 $3 0 }
  | id '.' id               { A.FieldVar (A.SimpleVar $1 0) $3 0 }
  | lvaluePrime '[' exp ']' { A.SubscriptVar $1 $3 0 }
  | id '[' exp ']'          { A.SubscriptVar (A.SimpleVar $1 0) $3 0 }

recordFields :: { [(A.Symbol, A.Exp, A.Pos)] }
  : id '=' exp recordFieldsTail { ($1, $3, 0) : $4 }
  | {- empty -}                 { [] }

recordFieldsTail :: { [(A.Symbol, A.Exp, A.Pos)] }
  : ',' id '=' exp recordFieldsTail { ($2, $4, 0) : $5 }
  | {- empty -}                     { [] }

decs :: { [A.Dec] }
  : dec decs { $1 : $2 }
  | {- empty -} { [] }

dec :: { A.Dec }
  : type id '=' ty                          { A.TypeDec{A.name=$2, A.ty=$4} }
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

vardec :: { A.Dec }
  : var id optTypeId ':=' exp { A.VarDec{A.name=$2, A.varDecTyp=$3, A.decInit=$5} }

optTypeId :: { Maybe (A.Symbol, A.Pos) }
  : ':' typeId  { Just ($2, 0) }
  | {- empty -} { Nothing }

ty :: { A.Ty }
  : typeId           { A.NameTy ($1, 0) }
  | '{' tyFields '}' { A.RecordTy $2 }

tyFields :: { [A.Field] }
  : id ':' typeId tyFieldTail { field $1 $3 : $4 }
  | {- empty -}               { [] }

tyFieldTail :: { [A.Field] }
  : ',' id ':' typeId tyFieldTail { field $2 $4 : $5 }
  | {- empty -}                   { [] }

typeId :: { A.Symbol }
  : id { $1 }

exps :: { A.Exp }
  : exp expsTail { seqConcat $1 $2 }
  | {- empty -}  { A.SeqExp [] }

expsTail :: { A.Exp }
  : ';' exp expsTail { seqConcat $2 $3 }
  | {- empty -}      { A.SeqExp [] }

elseTail :: { Maybe A.Exp }
  : else exp    { Just $2 }
  | {- empty -} { Nothing }

callArgs :: { [A.Exp] }
  : exp callArgs { $1 : $2 }
  | {- empty -}  { [] }

{
seqConcat :: A.Exp -> A.Exp -> A.Exp
seqConcat exp (A.SeqExp (exp':exps)) = A.SeqExp ((exp, 0):(exp':exps))
seqConcat exp (A.SeqExp []) = A.SeqExp [(exp, 0)]

field :: A.Symbol -> A.Symbol -> A.Field
field fieldName' fieldTyp' = A.Field{A.fieldName=fieldName', A.fieldTyp=fieldTyp'}

funDec :: A.Symbol -> [A.Field] -> Maybe(A.Symbol, A.Pos) -> A.Dec
funDec name fields maybeTy = A.FunctionDec [A.FunDec{A.fundecName=name, A.params=fields, A.result=maybeTy}]

parseError :: [L.Lexeme] -> a
parseError [] = error "Parse Error at EOF"
parseError (x:xs) = error ("Parse Error at token " ++ show x)

posn :: L.Lexeme -> A.Posn
posn l = case L.tokPosn l of
           L.AlexPn a b c -> A.Posn{A.absChrOffset=a, A.lineno=b, A.colno=c}

parse :: String -> Either String A.Exp
parse input = L.scanner input >>= calc

}
