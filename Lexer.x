{
module Lexer where
}

%wrapper "posn"

$digit = 0-9 -- digits
$alpha = [a-zA-Z] -- alphabetic characters

tokens :-

  $white+                                 ;
  "--".*                                  ;
  type                                    { \s p -> Type }
  var                                     { \s p -> Var }
  function+                               { \s p -> Function }
  break                                   { \s p -> Break }
  of                                      { \s p -> Of }
  nil                                     { \s p -> Nil }
  do                                      { \s p -> Do }
  array                                   { \s p -> Array }
  if                                      { \s p -> If }
  then                                    { \s p -> Then }
  else                                    { \s p -> Else }
  while                                   { \s p -> While }
  for                                     { \s p -> For }
  to                                      { \s p -> To }
  let                                     { \s p -> Let }
  in                                      { \s p -> In }
  end                                     { \s p -> End }
  class                                   { \s p -> Class }
  extends                                 { \s p -> Extends }
  method                                  { \s p -> Method }
  new                                     { \s p -> New }
  "|"                                     { \s p -> Or }
  "&"                                     { \s p -> And }
  ">="                                    { \s p -> GE }
  "<="                                    { \s p -> LE }
  "<"                                     { \s p -> LT' }
  "<>"                                    { \s p -> NEq }
  "="                                     { \s p -> Eq }
  "/"                                     { \s p -> Divide }
  "*"                                     { \s p -> Times }
  "-"                                     { \s p -> Minus }
  "+"                                     { \s p -> Plus }
  "."                                     { \s p -> Dot }
  "}"                                     { \s p -> RBrace }
  "{"                                     { \s p -> LBrace }
  "]"                                     { \s p -> RBrack }
  "["                                     { \s p -> LBrack }
  ")"                                     { \s p -> RParen }
  "("                                     { \s p -> LParen }
  ";"                                     { \s p -> Semicolon }
  ":"                                     { \s p -> Colon }
  ","                                     { \s p -> Comma }

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
      Type
    | Var
    | Function
    | Break
    | Of
    | Nil
    | Do
    | Array
    | If
    | Then
    | Else
    | While
    | For
    | To
    | Let
    | In
    | End
    | Class
    | Extends
    | Method
    | New
    | Or
    | And
    | GE
    | LE
    | LT'
    | NEq
    | Eq
    | Divide
    | Times
    | Minus
    | Dot
    | RBrace
    | LBrace
    | RBrack
    | LBrack
    | RParen
    | LParen
    | Semicolon
    | Colon
    | Comma
    | Plus
    deriving (Eq,Show)
}
