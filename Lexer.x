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

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
    Type |
    Var |
    Function |
    Break |
    Of |
    Nil |
    Do |
    Array |
    If |
    Then |
    Else |
    While |
    For |
    To |
    Let |
    In |
    End
    deriving (Eq,Show)
}
