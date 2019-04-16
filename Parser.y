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
  id        { L.Lexeme _ (L.ID _) _ }
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
%nonassoc do
%nonassoc else
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
  | id '{' recordFields '}'        { recordExp $3 $1 }
  | id '[' exp ']' of exp          { arrayExp $1 $3 $6 }
  | id '(' callArgs ')'            { callExp $1 $3 $4 }
--| lvalue '.' id '(' callArgs ')' { A.Method ... }
  | exp '+' exp                    { opExp $1 A.PlusOp $3 $2 }
  | exp '-' exp                    { opExp $1 A.MinusOp $3 $2 }
  | exp '*' exp                    { opExp $1 A.TimesOp $3 $2 }
  | exp '/' exp                    { opExp $1 A.DivideOp $3 $2 }
  | exp '>=' exp                   { opExp $1 A.GeOp $3 $2 }
  | exp '<=' exp                   { opExp $1 A.LeOp $3 $2 }
  | exp '=' exp                    { opExp $1 A.EqOp $3 $2 }
  | exp '<>' exp                   { opExp $1 A.NeqOp $3 $2 }
  | exp '<' exp                    { opExp $1 A.LtOp $3 $2 }
  | exp '>' exp                    { opExp $1 A.GtOp $3 $2 }
  | exp '|' exp                    { orExp $1 $3 $2 }
  | exp '&' exp                    { andExp $1 $3 $2 }
  | '-' exp %prec UMINUS           { opExp (A.IntExp 0) A.MinusOp $2 $1 }
  | '(' exps ')'                   { mkSeq $2 $1 }
  | if exp then exp elseTail       { A.IfExp{A.test=$2,
                                             A.then'=$4,
                                             A.else'=$5,
                                             A.pos=(posn $1)} }
  | while exp do exp               { A.WhileExp{A.test=$2,
                                                A.body=$4,
                                                A.pos=(posn $1)} }
  | for id ':=' exp to exp do exp  { A.ForExp{A.forVar=(identifier $2),
                                              A.lo=$4,
                                              A.hi=$6,
                                              A.body=$8,
                                              A.pos=(posn $1)} }
  | break                          { A.BreakExp (posn $1) }
  | let decs in exps end           { A.LetExp{A.decs=(mergeAdjacent $2),
                                              A.body=(mkSeq $4 $3),
                                              A.pos=(posn $1)} }

decs :: { [A.Dec] }
  : dec decs { $1 : $2 }
  | {- empty -} { [] }

dec :: { A.Dec }
  : type id '=' ty                          { typeDec $1 $2 $4 }
--| class id [ extends type-id ]            { classfields }
  | vardec                                  { $1 }
  | function id '(' tyFields ')' optTypeId  { funDec (identifier $2) $4 $6 }
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
  : var id optTypeId ':=' exp { A.VarDec{A.name=(identifier $2),
                                         A.varDecTyp=$3,
                                         A.decInit=$5,
                                         A.decPos=(posn $1)} }

optTypeId :: { Maybe (A.Symbol, A.Pos) }
  : ':' id  { Just (identifier $2, posn $2) }
  | {- empty -} { Nothing }

ty :: { A.Ty }
  : id           { A.NameTy (identifier $1, posn $1) }
  | '{' tyFields '}' { A.RecordTy $2 }

tyFields :: { [A.Field] }
  : id ':' id tyFieldTail { field (identifier $1) $3 : $4 }
  | {- empty -}           { [] }

tyFieldTail :: { [A.Field] }
  : ',' id ':' id tyFieldTail { field (identifier $2) $4 : $5 }
  | {- empty -}               { [] }

elseTail :: { Maybe A.Exp }
  : else exp { Just $2 }
  | %prec do { Nothing }

exps :: { Maybe (A.Exp, [(A.Exp, A.Pos)]) }
  : exp expsTail { Just ($1, $2) }
  | {- empty -}  { Nothing }

expsTail :: { [(A.Exp, A.Pos)] }
  : ';' exp expsTail { ($2, posn $1) : $3 }
  | {- empty -}      { [] }

callArgs :: { [A.Exp] }
  : exp callArgsTail { $1 : $2 }
  | {- empty -}  { [] }

callArgsTail :: { [A.Exp] }
  : ',' exp callArgsTail { $2 : $3 }
  | {- empty -}  { [] }

recordFields :: { [(A.Symbol, A.Exp, A.Pos)] }
  : id '=' exp recordFieldsTail { (recordField $1 $3 $2) : $4 }
  | {- empty -}                 { [] }

recordFieldsTail :: { [(A.Symbol, A.Exp, A.Pos)] }
  : ',' id '=' exp recordFieldsTail { (recordField $2 $4 $3) : $5 }
  | {- empty -}                     { [] }

lvalue :: { A.Var }
  : id          { simpleVar $1 }
  | lvaluePrime { $1 }

lvaluePrime :: { A.Var }
  : id '.' id               { fieldVar (simpleVar $1) $3 }
  | lvaluePrime '.' id      { fieldVar $1 $3 }
  | id '[' exp ']'          { subscriptVar (simpleVar $1) $3 $4 }
  | lvaluePrime '[' exp ']' { subscriptVar $1 $3 $4 }

{
mergeAdjacent :: [A.Dec] -> [A.Dec]
mergeAdjacent decls = mergeAdjacentFuncs $ mergeAdjacentTys decls

mergeAdjacentFuncs :: [A.Dec] -> [A.Dec]
mergeAdjacentFuncs declist = fst $ merge ([], []) declist where
    merge (mergedSoFar, funDecs) (A.FunctionDec(funDecs2) : decls) = merge (mergedSoFar, funDecs ++ funDecs2) decls
    merge (mergedSoFar, funDecs) (decl : decls) = merge (mergedSoFar ++ [A.FunctionDec funDecs] ++ [decl], []) decls
    merge (mergedSoFar, funDecs) [] = (mergedSoFar ++ [A.FunctionDec funDecs], [])

mergeAdjacentTys :: [A.Dec] -> [A.Dec]
mergeAdjacentTys declist = fst $ merge ([], []) declist where
    merge (mergedSoFar, tydecs) (A.TypeDec(tydecs2) : decls) = merge (mergedSoFar, tydecs ++ tydecs2) decls
    merge (mergedSoFar, tydecs) (decl : decls) = merge (mergedSoFar ++ [A.TypeDec tydecs] ++ [decl], []) decls
    merge (mergedSoFar, tydecs) [] = (mergedSoFar ++ [A.TypeDec tydecs], [])

mkSeq :: Maybe (A.Exp, [(A.Exp, A.Pos)]) -> L.Lexeme -> A.Exp
mkSeq m l = case m of
              Just (e, es) -> A.SeqExp $ (e, posn l) : es
              Nothing -> A.SeqExp []

opExp :: A.Exp -> A.Oper -> A.Exp -> L.Lexeme -> A.Exp
opExp e1 op e2 l = A.OpExp{A.left=e1, A.oper=op, A.right=e2, A.pos=(posn l)}

andExp :: A.Exp -> A.Exp -> L.Lexeme -> A.Exp
andExp e1 e2 l = A.IfExp{A.test=e1,
                         A.then'=e2,
                         A.else'=(Just $ A.IntExp 0),
                         A.pos=(posn l)}

orExp :: A.Exp -> A.Exp -> L.Lexeme -> A.Exp
orExp e1 e2 l = A.IfExp{A.test=e1,
                        A.then'=(A.IntExp 1),
                        A.else'=(Just e2),
                        A.pos=(posn l)}

callExp :: L.Lexeme -> [A.Exp] -> L.Lexeme -> A.Exp
callExp func args l = A.CallExp{A.func=(identifier func),
                                A.args=args,
                                A.pos=(posn l)}

arrayExp :: L.Lexeme -> A.Exp -> A.Exp -> A.Exp
arrayExp l size init = A.ArrayExp{A.typ=(identifier l),
                                  A.size=size,
                                  A.init=init,
                                  A.pos=(posn l)}

identifier :: L.Lexeme -> A.Symbol
identifier (L.Lexeme _ (L.ID id) _) = id

recordField :: L.Lexeme -> A.Exp -> L.Lexeme -> (A.Symbol, A.Exp, A.Pos)
recordField id e l = (identifier id, e, posn l)

recordExp :: [(A.Symbol, A.Exp, A.Pos)] -> L.Lexeme -> A.Exp
recordExp fields l = A.RecordExp{A.fields=fields,
                                 A.typ=(identifier l),
                                 A.pos=(posn l)}

simpleVar :: L.Lexeme -> A.Var
simpleVar l = A.SimpleVar (identifier l) (posn l)

field :: A.Symbol -> L.Lexeme -> A.Field
field name l = A.Field{A.fieldName=name,
                       A.fieldTyp=(identifier l),
                       A.fieldPos=(posn l)}

fieldVar :: A.Var -> L.Lexeme -> A.Var
fieldVar var l = A.FieldVar var (identifier l) (posn l)

typeDec :: L.Lexeme -> L.Lexeme -> A.Ty -> A.Dec
typeDec l1 l2 t = A.TypeDec [A.TyDec {A.tydecName=(identifier l2),
                                      A.ty=t,
                                      A.tydecPos=(posn l1)}]

funDec :: A.Symbol -> [A.Field] -> Maybe(A.Symbol, A.Pos) -> A.Dec
funDec name fields maybeTy = A.FunctionDec [A.FunDec{A.fundecName=name,
                                                     A.params=fields,
                                                     A.result=maybeTy}]

subscriptVar :: A.Var -> A.Exp -> L.Lexeme -> A.Var
subscriptVar v e l = A.SubscriptVar v e (posn l)

parseError :: [L.Lexeme] -> a
parseError [] = error "Parse Error at EOF"
parseError (x:xs) = error ("Parse Error at token " ++ show x)

posn :: L.Lexeme -> A.Pos
posn l = case L.tokPosn l of
           L.AlexPn a b c -> A.Pos{A.absChrOffset=a, A.lineno=b, A.colno=c}

parse :: String -> Either String A.Exp
parse input = L.scanner input >>= calc

}
