{
module Lexer where
}

%wrapper "posn"

@keyword = type|var|function|break|of|nil|do|array|if|then|else|
           while|for|to|let|in|end

@object_keyword = class|extends|method|new

@symbol = "|" | "&" | ">=" | "<=" | "<" | ">" | "<>" | "=" |
          "/" | "*" | "-" | "+" | ":=" | "." | "(" | ")" |
          "[" | "]" | "{" | "}" | "," | ":" | ";"

$digit = 0-9
$letter = [a-zA-Z]
@id = $letter ($letter | $digit | _) | _main
@decimal     = $digit+

tokens :-

  $white+           ;
  @keyword          { \s p -> LKeyword }
  @object_keyword   { \s p -> LObjectKeyword }
  @symbol           { \s p -> LSymbol }
  @decimal          { \s p -> LInteger }
  @id               { \s p -> LIdentifier }

{
-- Each action has type :: String -> Token

-- The token type:
data LexemeClass =
      LKeyword
    | LObjectKeyword
    | LSymbol
    | LInteger
    | LIdentifier
    deriving (Eq,Show)
}
