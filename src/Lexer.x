-- Modified very slightly from Simon Marlow's implementation:

-- https://github.com/simonmar/alex/master/examples/tiger.x

{
{-# OPTIONS -w -funbox-strict-fields #-}
module Lexer where

import Prelude hiding ( GT, LT, EQ )
import System.Console.GetOpt ( OptDescr(..), ArgOrder(..), getOpt, usageInfo )
import Control.Monad
import Data.Maybe
import Numeric ( readDec )
import Data.Char ( chr )
import Data.Map ( Map )
import Data.List ( intercalate )
import System.Environment ( getArgs, getProgName )
import qualified Data.Map as Map ( empty )
}

%wrapper "monadUserState"

$whitespace = [\ \t\b]
$digit      = 0-9                                            -- digits
$alpha      = [A-Za-z]
$letter     = [a-zA-Z]                                       -- alphabetic characters
$ident      = [$letter $digit _]                             -- identifier character

@number     = [$digit]+
@identifier = $alpha($alpha|_|$digit)* | _main


state:-

<0>            "type"        { mkL TYPE }
<0>            "var"         { mkL VAR }
<0>            "function"    { mkL FUNCTION }
<0>            "break"       { mkL BREAK }
<0>            "of"          { mkL OF }
<0>            "end"         { mkL END }
<0>            "in"          { mkL IN }
<0>            "nil"         { mkL NIL }
<0>            "let"         { mkL LET }
<0>            "do"          { mkL DO }
<0>            "to"          { mkL TO }
<0>            "for"         { mkL FOR }
<0>            "while"       { mkL WHILE }
<0>            "else"        { mkL ELSE }
<0>            "then"        { mkL THEN }
<0>            "if"          { mkL IF }
<0>            "array"       { mkL ARRAY }
<0>            "exception"   { mkL EXCEPTION }
<0>            "handle"      { mkL HANDLE }
<0>            "import"      { mkL IMPORT }
<0>            "primitive"   { mkL PRIMITIVE }
<0>             :\=          { mkL ASSIGN }
<0>             \|           { mkL OR }
<0>             &            { mkL AND }
<0>             \>\=         { mkL GE }
<0>             \>           { mkL GT }
<0>             \<\=         { mkL LE }
<0>             \<           { mkL LT }
<0>             \<\>         { mkL NEQ }
<0>             \=           { mkL EQ }
<0>             \/           { mkL DIVIDE }
<0>             \*           { mkL TIMES }
<0>             \-           { mkL MINUS }
<0>             \+           { mkL PLUS }
<0>             \.           { mkL DOT }
<0>             \}           { mkL RBRACE }
<0>             \{           { mkL LBRACE }
<0>             \[           { mkL LBRACK }
<0>             \]           { mkL RBRACK }
<0>             \)           { mkL RPAREN }
<0>             \(           { mkL LPAREN }
<0>             \;           { mkL SEMICOLON }
<0>             :            { mkL COLON }
<0>             ","          { mkL COMMA }
<0>             "/*"         { enterNewComment `andBegin` state_comment }
<state_comment> "/*"         { embedComment }
<state_comment> "*/"         { unembedComment }
<state_comment> .            ;
<state_comment> \n           { skip }
<0>             \"           { enterNewString `andBegin` state_string }
<state_string>  \\n          { addCharToString '\n' }
<state_string>  \\t          { addCharToString '\t' }

<state_string>  \\\^[@-_]    { addControlToString }
<state_string>  \\$digit$digit$digit
                             { addAsciiToString }
<state_string>  \\\"         { addCharToString '\"' }
<state_string>  \\\\         { addCharToString '\\' }
<state_string>  \\[\ \n\t\f\r\b\v]+\\
                             ;
<state_string>  \\           { \_ _ -> lexerError "Illegal escape sequence" }
<state_string>  \"           { leaveString `andBegin` state_initial }
<state_string>  .            { addCurrentToString }
<state_string>  \n           { skip }
<0>             \n           { skip }
<0>             $whitespace+ ;
<0>             @number      { getInteger }
<0>             @identifier  { getVariable }
<0>             .            { \_ _ -> lexerError "Illegal character" }

{
-- The token type

data Lexeme = Lexeme AlexPosn LexemeClass (Maybe String)

instance Show Lexeme where
  show (Lexeme _ EOF _)   = "  Lexeme EOF"
  show (Lexeme p cl  mbs) = "(" ++ (intercalate ", " ["Lexeme class=" ++ show cl, showap p, showst mbs]) ++ ")"
    where
      showap pp = "posn=" ++ showPosn pp
      showst Nothing  = ""
      showst (Just s) = "string=" ++ show s

tokPosn :: Lexeme -> AlexPosn
tokPosn (Lexeme p _ _) = p

data LexemeClass =
        EOF
      | ID         String
      | INT        Int
      | STRING     String
      | COMMA
      | COLON
      | SEMICOLON
      | LPAREN
      | RPAREN
      | LBRACK
      | RBRACK
      | LBRACE
      | RBRACE
      | DOT
      | PLUS
      | MINUS
      | TIMES
      | DIVIDE
      | EQ
      | NEQ
      | LT
      | LE
      | GT
      | GE
      | AND
      | OR
      | ASSIGN
      | ARRAY
      | IF
      | THEN
      | ELSE
      | WHILE
      | FOR
      | TO
      | DO
      | LET
      | IN
      | END
      | OF
      | BREAK
      | NIL
      | FUNCTION
      | VAR
      | TYPE
      | UNARYMINUS
      | EXCEPTION
      | TRY
      | HANDLE
      | RAISE
      | IMPORT
      | PRIMITIVE
      deriving (Show, Eq)

mkL :: LexemeClass -> AlexInput -> Int -> Alex Lexeme
mkL c (p, _, _, str) len = pure (Lexeme p c (Just (take len str)))

-- states

state_initial :: Int
state_initial = 0

-- actions

enterNewComment, embedComment, unembedComment :: Action
enterNewString, leaveString, addCurrentToString, addAsciiToString, addControlToString :: Action
getInteger, getVariable :: Action

enterNewComment input len =
    do setLexerCommentDepth 1
       skip input len

embedComment input len =
    do cd <- getLexerCommentDepth
       setLexerCommentDepth (cd + 1)
       skip input len

unembedComment input len =
    do cd <- getLexerCommentDepth
       setLexerCommentDepth (cd - 1)
       when (cd == 1) (alexSetStartCode state_initial)
       skip input len

enterNewString _     _   =
    do setLexerStringState True
       setLexerStringValue ""
       alexMonadScan

addCharToString :: Char -> Action
addCharToString c _     _   =
    do addCharToLexerStringValue c
       alexMonadScan

addCurrentToString i@(_, _, _, input) len = addCharToString c i len
  where
    c = if (len == 1)
           then head input
           else error "Invalid call to addCurrentToString''"

-- if we are given the special form '\nnn'
addAsciiToString i@(_, _, _, input) len = if (v < 256)
                                          then addCharToString c i len
                                          else lexerError ("Invalid ascii value : " ++ input)
  where
    s = if (len == 4)
           then drop 1 input
           else error "Invalid call to 'addAsciiToString'"
    r = readDec s
    v = if (length r == 1)
           then fst (head r)
           else error "Invalid call to 'addAsciiToString'"
    c = chr v

-- if we are given the special form '\^A'
addControlToString i@(_, _, _, input) len = addCharToString c' i len
  where
    c = if (len == 1)
           then head input
           else error "Invalid call to 'addControlToString'"
    v = ord c
    c' = if (v >= 64)
            then chr (v - 64)
            else error "Invalid call to 'addControlToString'"

leaveString (p, _, _, input) len =
    do s <- getLexerStringValue
       setLexerStringState False
       pure (Lexeme p (STRING (reverse s)) (Just (take len input)))

getInteger (p, _, _, input) len = if (length r == 1)
                                  then pure (Lexeme p (INT (fst (head r))) (Just s))
                                  else lexerError "Invalid number"
  where
    s = take len input
    r = readDec s

-- a sequence of letters is an identifier, except for reserved words, which are tested for beforehand
getVariable (p, _, _, input) len = pure (Lexeme p (ID s) (Just s))
  where
    s = take len input


-- The user state monad

data AlexUserState = AlexUserState
                   {
                     -- used by the lexer phase
                       lexerCommentDepth  :: Int
                     , lexerStringState   :: Bool
                     , lexerStringValue   :: String
                     -- used by the parser phase
                     , parserCollIdent    :: Map String Int
                     , parserCurrentToken :: Lexeme
                     , parserPos          :: Pos
                   }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
                   {
                       lexerCommentDepth  = 0
                     , lexerStringState   = False
                     , lexerStringValue   = ""
                     , parserCollIdent    = Map.empty
                     , parserCurrentToken = Lexeme undefined EOF Nothing
                     , parserPos          = Nothing
                   }

getLexerCommentDepth :: Alex Int
getLexerCommentDepth = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerCommentDepth ust)

setLexerCommentDepth :: Int -> Alex ()
setLexerCommentDepth ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerCommentDepth=ss}}, ())

getLexerStringState :: Alex Bool
getLexerStringState = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerStringState ust)

setLexerStringState :: Bool -> Alex ()
setLexerStringState ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerStringState=ss}}, ())

getLexerStringValue :: Alex String
getLexerStringValue = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, lexerStringValue ust)

setLexerStringValue :: String -> Alex ()
setLexerStringValue ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerStringValue=ss}}, ())

addCharToLexerStringValue :: Char -> Alex ()
addCharToLexerStringValue c = Alex $ \s -> Right (s{alex_ust=(alex_ust s){lexerStringValue=c:lexerStringValue (alex_ust s)}}, ())

getCollNameToIdent :: Alex (Map String Int)
getCollNameToIdent = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, parserCollIdent ust)

setCollNameToIdent :: Map String Int -> Alex ()
setCollNameToIdent ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){parserCollIdent=ss}}, ())

getParserCurrentToken :: Alex Lexeme
getParserCurrentToken = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, parserCurrentToken ust)

setParserCurrentToken :: Lexeme -> Alex ()
setParserCurrentToken ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){parserCurrentToken=ss}}, ())

getParserPos :: Alex Pos
getParserPos = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, parserPos ust)

setParserPos :: Pos -> Alex ()
setParserPos ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){parserPos=ss}}, ())

-- utilities

showPosn :: AlexPosn -> String
showPosn (AlexPn _ line col) = show line ++ ':': show col

type Pos     = Maybe AlexPosn

line_number :: Pos -> (Int, Int)
line_number Nothing                   = (0, 0)
line_number (Just (AlexPn _ lig col)) = (lig, col)

-- definition needed by Alex

alexEOF :: Alex Lexeme
alexEOF = pure (Lexeme undefined EOF Nothing)

-- Execution

scanner :: String -> Either String [Lexeme]
scanner str = let loop = do (t, m) <- alexComplementError alexMonadScan
                            when (isJust m) (lexerError (fromJust m))
                            let tok@(Lexeme _ cl _) = t
                            if (cl == EOF)
                               then do f1 <- getLexerStringState
                                       d2 <- getLexerCommentDepth
                                       if ((not f1) && (d2 == 0))
                                          then pure []
                                          else if (f1)
                                               then alexError "String not closed at end of file"
                                               else alexError "Comment not closed at end of file"
                               else do toks <- loop
                                       pure (tok : toks)
              in  runAlex str loop

-- we capture the error message in order to complement it with the file position
alexComplementError :: Alex a -> Alex (a, Maybe String)
alexComplementError (Alex al) = Alex (\s -> case al s of
                                                 Right (s', x) -> Right (s', (x, Nothing))
                                                 Left  message -> Right (s, (undefined, Just message)))

lexer :: (Lexeme -> Alex a) -> Alex a
lexer cont =
    do t <- lexToken
       setParserCurrentToken t  -- helps in producing informative error messages
       cont t

lexToken :: Alex Lexeme
lexToken =
    do
       inp <- alexGetInput
       sc <- alexGetStartCode
       case alexScan inp sc of
            AlexEOF              -> alexEOF
            AlexError _          -> alexError "lexical error"
            AlexSkip  inp1 _     -> do
                                       alexSetInput inp1
                                       lexToken
            AlexToken inp1 len t -> do
                                       alexSetInput inp1
                                       t inp len

lexerError :: String -> Alex a
lexerError msg =
    do (p, c, _, inp) <- alexGetInput
       let inp1 = filter (/= '\r') (takeWhile (/='\n') inp)
       let inp2 = if (length inp1 > 30)
                     then trim (take 30 inp1)
                     else trim inp1
       let disp = if (null inp)
                     then " at end of file"
                     else if (null inp2)
                             then " before end of line"
                             else " on char " ++ show c ++ " before : '" ++ inp2 ++ "'"
       let disp3 = if (null msg)
                      then "Lexer error"
                      else trim msg
       alexError (disp3 ++ " at " ++ showPosn p ++ disp)
  where
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

type Action = AlexInput -> Int -> Alex Lexeme

-- used by the parser: run lexer, parser & get the symbol table
runAlexTable :: String -> Alex a -> Either String (a, Map String Int)
runAlexTable input (Alex f)
   = case f (AlexState { alex_pos = alexStartPos
                       , alex_inp = input
                       , alex_chr = '\n'
                       , alex_scd = 0
                       , alex_ust = alexInitUserState }) of
            Left msg      -> Left msg
            Right (st, a) -> Right (a, parserCollIdent (alex_ust st))

}
