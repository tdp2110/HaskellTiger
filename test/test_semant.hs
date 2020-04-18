module Main where

import qualified Frame
import qualified Parser
import qualified Semant
import qualified Symbol
import qualified Temp
import qualified Translate
import qualified TreeIR
import qualified Types
import qualified X64Frame

import           Test.HUnit
import           System.Exit
import qualified Data.DList                    as DList
import           Data.Either
import           Data.Foldable
import           Data.List
import           Prelude                 hiding ( exp )

import           Debug.Trace
debug :: a -> String -> a
debug = flip trace

parseToSema
  :: String
  -> Either
       Semant.SemantError
       (Semant.ExpTy, Semant.FragList, Temp.Generator, X64Frame.X64)
parseToSema text = let (Right ast) = Parser.parse text in Semant.transProg ast

intLiteral :: Test
intLiteral = TestCase
  (let text = "1337"
       (Right (Semant.ExpTy { Semant.exp = _, Semant.ty = ty }, _, _, _)) =
           parseToSema text
   in  assertEqual "int literal" Types.INT ty
  )

intArith1 :: Test
intArith1 = TestCase
  (let
     text = "42 + 1337"
     (Right (Semant.ExpTy { Semant.exp = (Translate.Ex exp), Semant.ty = ty }, _, _, _))
       = parseToSema text
   in
     do
       assertEqual "int arith 1" Types.INT ty
       assertEqual "arith" exp $ TreeIR.CONST $ 42 + 1337
  )

intArith2 :: Test
intArith2 = TestCase
  (let text = "42 * 1337"
       (Right (Semant.ExpTy { Semant.exp = _, Semant.ty = ty }, _, _, _)) =
           parseToSema text
   in  assertEqual "int arith 2" Types.INT ty
  )

intArith3 :: Test
intArith3 = TestCase
  (let text = "(1 + 2 - (3 / (4 - 5) - 6 *(7 + 8)))"
       (Right (Semant.ExpTy { Semant.exp = _, Semant.ty = ty }, _, _, _)) =
           parseToSema text
   in  assertEqual "int arith 3" Types.INT ty
  )

strLiteral :: Test
strLiteral = TestCase
  (let
     str1    = "hello world"
     literal = "\"" ++ str1 ++ "\""
     (Right (Semant.ExpTy { Semant.exp = expr, Semant.ty = ty }, frags, _, _))
       = parseToSema literal

     (Translate.Ex (TreeIR.ESEQ (TreeIR.MOVE (TreeIR.TEMP t1, TreeIR.CALL (TreeIR.NAME (Temp.Label (Symbol.Symbol "_tiger_allocString")), [TreeIR.NAME lab1, TreeIR.CONST strlen], [Frame.DoesNotEscape, Frame.DoesNotEscape], True)), TreeIR.TEMP t2)))
       = expr

     [X64Frame.STRING (lab2, str2)] = DList.toList frags
   in
     do
       assertEqual "labels match"   lab1         lab2
       assertEqual "literals match" str1         str2
       assertEqual "str literal"    Types.STRING ty
       assertEqual "strlen arg is correct" strlen $ length str1
       assertEqual "temp labels match up" t1 t2
  )

strPlusIntIsErr :: Test
strPlusIntIsErr = TestCase
  (let text       = "\"hello world\" + 2"
       semaResult = parseToSema text
   in  assertBool "can't add strings and ints" $ isLeft semaResult
  )

substringCall1 :: Test
substringCall1 = TestCase
  (let text = "substring(\"hello world\", 0, 1)"
       (Right (Semant.ExpTy _ ty, _, _, _)) = parseToSema text
   in  assertEqual "substring returns string" Types.STRING ty
  )

substringCall2 :: Test
substringCall2 = TestCase
  (let text = "substring(\"hello world\", nil, 1337)"
       (Left (Semant.SemantError err _)) = parseToSema text
   in  assertBool "wrongly typed param" $ isInfixOf "parameter 1" err
  )

substringCall3 :: Test
substringCall3 = TestCase
  (let text = "substring(\"hello world\", 42, 1337, nil)"
       (Left (Semant.SemantError err _)) = parseToSema text
   in  assertBool "wrong number of arguments"
         $ isInfixOf "expects 2 parameters but was passed 3" err
  )

letExp1 :: Test
letExp1 = TestCase
  (let text =
           "let type Int = int\n"
             ++ "    var n := 0\n"
             ++ "    var m : Int := n + 1 in \n"
             ++ "  let var k := 1 in \n"
             ++ "    n := n + m;\n"
             ++ "    if n = 0 then m - n + 2 * k\n"
             ++ "    else n * (m + k)\n"
             ++ "  end\n"
             ++ "end"
       (Right (Semant.ExpTy { Semant.exp = _, Semant.ty = ty }, _, _, _)) =
           parseToSema text
   in  assertEqual "letExp example1" Types.INT ty
  )

intUncallable :: Test
intUncallable = TestCase
  (let text                              = "let var x := 2 in x(3) end"
       (Left (Semant.SemantError err _)) = parseToSema text
   in  assertEqual "integers are not callable"
                   "only functions are callable: found type INT"
                   err
  )

forVar1 :: Test
forVar1 = TestCase
  (let text                              = "for j:=0 to 10 do j:=j+1"
       (Left (Semant.SemantError err _)) = parseToSema text
   in  assertEqual "can't assign to forVar" "forVar assigned in forBody" err
  )

forVar2 :: Test
forVar2 = TestCase
  (let text = "for j:=0 to 10 do (let var k := 2 in j := j + k end)"
       (Left (Semant.SemantError err _)) = parseToSema text
   in  assertEqual "can't assign to forVar" "forVar assigned in forBody" err
  )

break1 :: Test
break1 = TestCase
  (let text = "for j:=0 to 10 do if j = 5 then break"
       (Right (Semant.ExpTy { Semant.exp = _, Semant.ty = ty }, _, _, _)) =
           parseToSema text
   in  assertEqual "break in forExp is ok" Types.UNIT ty
  )

nilRecord :: Test
nilRecord = TestCase
  (let text =
           "let\n"
             ++ "  type intPair = { fst: int, snd: int }\n"
             ++ "  var nilPair : intPair := nil\n"
             ++ "in nilPair end"
       (Right (Semant.ExpTy { Semant.exp = _, Semant.ty = ty }, _, _, _)) =
           parseToSema text
   in  assertBool "nil record has record type" $ isRecord ty
  )

isRecord :: Types.Ty -> Bool
isRecord (Types.RECORD _) = True
isRecord _                = False

listTy1 :: Test
listTy1 = TestCase
  (let text =
           "let\n"
             ++ "  type intList = { head: int, tail: intList}\n"
             ++ "  var nilIntList : intList := nil\n"
             ++ "  var xs := intList{head = 0, tail = nilIntList}\n"
             ++ "in xs end"
       (Right (Semant.ExpTy { Semant.exp = _, Semant.ty = ty }, _, _, _)) =
           parseToSema text
   in  assertBool "instance of mutrec record has record ty" $ isRecord ty
  )

similarRecordDecs :: Test
similarRecordDecs = TestCase
  (let text =
           "let\n"
             ++ "  type pair1 = { fst: int, snd: int }\n"
             ++ "  type pair2 = { fst: int, snd: int }\n"
             ++ "  var p1:= pair1{ fst=1, snd=2 }\n"
             ++ "  var p2:= pair2{ fst=3, snd=4 }\n"
             ++ "in p1.fst + p2.snd end"
       (Right (Semant.ExpTy { Semant.exp = _, Semant.ty = ty }, _, _, _)) =
           parseToSema text
   in  assertEqual "similar records" Types.INT ty
  )

arrayOfTypeAlias :: Test
arrayOfTypeAlias = TestCase
  (let text =
           "let\n"
             ++ "  var N:= 16\n"
             ++ "  type intAlias = int\n"
             ++ "  type intArray = array of int\n"
             ++ "  var xs := intArray [N] of 0\n"
             ++ "in xs[8] end"
       (Right (Semant.ExpTy { Semant.exp = _, Semant.ty = ty }, _, _, _)) =
           parseToSema text
   in  assertEqual "similar records" Types.INT ty
  )

break2 :: Test
break2 = TestCase
  (let text                              = "if 1 then break"
       (Left (Semant.SemantError err _)) = parseToSema text
   in  assertEqual "can't assign to forVar"
                   "break expression not enclosed in a while or for"
                   err
  )

illegalDecls1 :: Test
illegalDecls1 = TestCase
  (let
     text =
       "let \n" ++ "  var N:= 0 \n" ++ "  function N() = nil \n" ++ "in N() end"
     (Left (Semant.SemantError err _)) = parseToSema text
   in
     assertBool "multiple declarations 1" $ isInfixOf
       "multiple function or value declarations of symbol N in letExp declarations"
       err
  )

illegalDecls2 :: Test
illegalDecls2 = TestCase
  (let
     text =
       "let \n" ++ "  var N:= 0 \n" ++ "  var N := 42 \n" ++ "in N + 1 end"
     (Left (Semant.SemantError err _)) = parseToSema text
   in
     assertBool "multiple declarations 1" $ isInfixOf
       "multiple function or value declarations of symbol N in letExp declarations"
       err
  )

illegalDecls3 :: Test
illegalDecls3 = TestCase
  (let
     text = "let \n" ++ "  var N: string:= 0 \n" ++ "in N + 1 end"
     (Left (Semant.SemantError err _)) = parseToSema text
   in
     assertEqual
       "annotation error"
       "mismatch in type annotation and computed type in varDecl: type annotation STRING, computed type INT"
       err
  )

illegalDecls4 :: Test
illegalDecls4 = TestCase
  (let text = "let \n" ++ "  var x:= nil \n" ++ "in x end"
       (Left (Semant.SemantError err _)) = parseToSema text
   in  assertEqual
         "nil decls need record annotation 1"
         "nil expression declarations must be constrained by a RECORD type"
         err
  )

illegalDecls5 :: Test
illegalDecls5 = TestCase
  (let text = "let \n" ++ "  var x:int:= nil \n" ++ "in x end"
       (Left (Semant.SemantError err _)) = parseToSema text
   in  assertEqual
         "nil decls need record annotation 2"
         "nil expression declarations must be constrained by a RECORD type"
         err
  )

illegalDecls6 :: Test
illegalDecls6 = TestCase
  (let
     text =
       "let\n"
         ++ "  var N := 8\n"
         ++ "  type notAnArrayTy = int\n"
         ++ "  var arr := notAnArrayTy [ N ] of 0"
         ++ " in arr[0] end"
     (Left (Semant.SemantError err _)) = parseToSema text
   in
     assertEqual
       "array decls must have an array type"
       "only array types may appear as the symbol in an array instance definition. Found type=INT"
       err
  )

illegalDecls7 :: Test
illegalDecls7 = TestCase
  (let text =
           "let\n"
             ++ "  var N := 8\n"
             ++ "  type intArray = array of int\n"
             ++ "  var arr := intArray [ N ] of \"hello\""
             ++ " in arr[0] end"
       (Left (Semant.SemantError err _)) = parseToSema text
   in  assertEqual
         "array init expr must have array elt type"
         "in ArrayExp, initExp has actual type STRING, when it must have INT"
         err
  )

illegalDecls8 :: Test
illegalDecls8 = TestCase
  (let text =
           "let\n"
             ++ "  type a = b\n"
             ++ "  type b = c\n"
             ++ "  type c = d\n"
             ++ "  type d = a\n"
             ++ "in 0 end"
       (Left (Semant.SemantError err _)) = parseToSema text
   in  assertBool "illegal cyclic type decl"
         $ isInfixOf "found illegal type declaration cycle" err
  )

mutuallyRecFuns :: Test
mutuallyRecFuns = TestCase
  (let text =
           "let\n"
             ++ "  function isOdd(x:int) : int = \n"
             ++ "    if x = 1 then 1 else isEven(x-1)\n"
             ++ "  function isEven(x:int) : int = \n"
             ++ "    if x = 0 then 1 else isOdd(x-1)\n"
             ++ "in isEven(140) end"
       (Right (Semant.ExpTy { Semant.exp = _, Semant.ty = ty }, frags, _, _)) =
           parseToSema text
   in  do
         assertEqual "mutrec isEven/Odd" Types.INT ty
         assertEqual "found twoo frags" 2 $ length frags
  )

isProc :: X64Frame.Frag -> Bool
isProc (X64Frame.PROC _ _) = True
isProc _                   = False

isString :: X64Frame.Frag -> Bool
isString (X64Frame.STRING _) = True
isString _                   = False

frags1 :: Test
frags1 = TestCase
  (let text =
           "let\n"
             ++ "  function isOdd(x:int) : int = \n"
             ++ "    if x = 1 then 1 else isEven(x-1)\n"
             ++ "  function isEven(x:int) : int = \n"
             ++ "    if x = 0 then 1 else isOdd(x-1)\n"
             ++ "in isEven(140) end"
       (Right (Semant.ExpTy { Semant.exp = _, Semant.ty = _ }, frags, _, _)) =
           parseToSema text
   in  do
         assertEqual "found two frags" 2 $ length frags
         assertBool "all frags are proc" $ all isProc frags
  )

frags2 :: Test
frags2 = TestCase
  (let text =
           "let\n"
             ++ "  var s1 := \"hello\"\n"
             ++ "  var s2 := \"world\"\n"
             ++ "in size(s1) + size(s2) end"
       (Right (Semant.ExpTy { Semant.exp = _, Semant.ty = _ }, frags, _, _)) =
           parseToSema text
   in  do
         assertEqual "found two frags" 2 $ length frags
         assertBool "all frags are string" $ all isString frags
         assertEqual "frags are coorect" ["hello", "world"] $ toList $ fmap
           (\(X64Frame.STRING (_, s)) -> s)
           frags
  )

frags3 :: Test
frags3 = TestCase
  (let
     text =
       "let\n"
         ++ "  function f(x:int) : int = \n"
         ++ "    let\n"
         ++ "      function g(y:int) : int = \n"
         ++ "        let function h(z:int) : int = x + y + z\n"
         ++ "            var s2 := \"dog\"\n"
         ++ "        in h(x) + size(s2) end\n"
         ++ "      var s := \"cat\""
         ++ "    in g(size(s)) end\n"
         ++ "in f(1337) end"
     (Right (Semant.ExpTy { Semant.exp = _, Semant.ty = _ }, fragsDList, _, _))
       = parseToSema text
     frags = toList fragsDList
   in
     do
       assertEqual "found 5 frags" 5 $ length frags
       assertEqual "3 procs" 3 $ length $ filter isProc frags
       assertEqual "string values" ["dog", "cat"]
         $   (\(X64Frame.STRING (_, s)) -> s)
         <$> filter isString frags
  )

ifExp1 :: Test
ifExp1 = TestCase
  (let
     text = "let\n" ++ "  var x := 0\n" ++ "in if x = 1 then x := 2\n end"
     (Right (Semant.ExpTy { Semant.exp = (Translate.Nx _), Semant.ty = ty }, _, _, _))
       = parseToSema text
   in
     assertEqual "well-formed if then exp has UNIT ty" Types.UNIT ty
  )

ifExp2 :: Test
ifExp2 = TestCase
  (let text = "let\n" ++ "  var x := 0\n" ++ "in if x = 1 then 2\n end"
       (Left (Semant.SemantError err _)) = parseToSema text
   in  assertBool "if then no else"
         $ isInfixOf "in if-then exp (without else), the if body" err
  )

ifExp3 :: Test
ifExp3 = TestCase
  (let text = "let\n" ++ "  var x := 0\n" ++ "in if x = 1 then 2 else 3\n end"
       (Right (Semant.ExpTy { Semant.exp = _, Semant.ty = ty }, _, _, _)) =
           parseToSema text
   in  assertEqual "well-formed if then else types match" Types.INT ty
  )

ifExp4 :: Test
ifExp4 = TestCase
  (let text =
           "let\n" ++ "  var x := 0\n" ++ "in if x = 1 then 2 else \"cat\"\n end"
       (Left (Semant.SemantError err _)) = parseToSema text
   in  assertBool "if then else types don't match"
         $ isInfixOf "incompatible types found in ifExp" err
  )

while1 :: Test
while1 = TestCase
  (let
     text =
       "let\n"
         ++ "  var x := 0\n"
         ++ "in\n"
         ++ "  while x < 10 do\n"
         ++ "    x := x + 1\n"
         ++ "end"
     (Right (Semant.ExpTy { Semant.exp = (Translate.Nx _), Semant.ty = ty }, _, _, _))
       = parseToSema text
   in
     assertEqual "well-formed while has UNIT ty" Types.UNIT ty
  )

while2 :: Test
while2 = TestCase
  (let
     text =
       "let\n"
         ++ "  var x := 0\n"
         ++ "in\n"
         ++ "  while x < 10 do\n"
         ++ "    (x := x + 1;\n"
         ++ "    if x = 5 then break)\n"
         ++ "end"
     (Right (Semant.ExpTy { Semant.exp = (Translate.Nx _), Semant.ty = ty }, _, _, _))
       = parseToSema text
   in
     assertEqual "well-formed while has UNIT ty" Types.UNIT ty
  )

while3 :: Test
while3 = TestCase
  (let text =
           "let\n"
             ++ "  var x := 0\n"
             ++ "in\n"
             ++ "  while x < 10 do\n"
             ++ "    (x := x + 1;\n"
             ++ "    2)\n"
             ++ "end"
       (Left (Semant.SemantError err _)) = parseToSema text
   in  assertBool "ill-formed while-body" $ isInfixOf
         "in a whileExp, the body expression must yield no value"
         err
  )

builtinFuncs :: Test
builtinFuncs = TestCase
  (let text = "let\n" ++ "  var x := 42" ++ "in print(chr(x)) end"
       (Right (Semant.ExpTy { Semant.exp = _, Semant.ty = ty }, _, _, _)) =
           parseToSema text
   in  assertEqual "" Types.UNIT ty
  )

tests :: Test
tests = TestList
  [ TestLabel "ints"              intLiteral
  , TestLabel "int arith 1"       intArith1
  , TestLabel "int arith 2"       intArith2
  , TestLabel "int arith 3"       intArith3
  , TestLabel "str literal"       strLiteral
  , TestLabel "str plus int"      strPlusIntIsErr
  , TestLabel "substring1"        substringCall1
  , TestLabel "nilRecord"         nilRecord
  , TestLabel "listTy1"           listTy1
  , TestLabel "similarRecordDecs" similarRecordDecs
  , TestLabel "break1"            break1
  , TestLabel "break2"            break2
  , TestLabel "forVar1"           forVar1
  , TestLabel "forVar2"           forVar2
  , TestLabel "illegalDecls1"     illegalDecls1
  , TestLabel "illegalDecls2"     illegalDecls2
  , TestLabel "illegalDecls3"     illegalDecls3
  , TestLabel "illegalDecls4"     illegalDecls4
  , TestLabel "illegalDecls5"     illegalDecls5
  , TestLabel "illegalDecls6"     illegalDecls6
  , TestLabel "illegalDecls7"     illegalDecls7
  , TestLabel "illegalDecls8"     illegalDecls8
  , TestLabel "substring2"        substringCall2
  , TestLabel "substring3"        substringCall2
  , TestLabel "letExp1"           letExp1
  , TestLabel "arrayOfTypeAlias"  arrayOfTypeAlias
  , TestLabel "intUncallable"     intUncallable
  , TestLabel "mutuallyRecFuns"   mutuallyRecFuns
  , TestLabel "ifExp1"            ifExp1
  , TestLabel "ifExp2"            ifExp2
  , TestLabel "ifExp3"            ifExp3
  , TestLabel "ifExp4"            ifExp4
  , TestLabel "while1"            while1
  , TestLabel "while2"            while2
  , TestLabel "while3"            while3
  , TestLabel "builtinFuncs"      builtinFuncs
  , TestLabel "frags1"            frags1
  , TestLabel "frags2"            frags2
  , TestLabel "frags3"            frags3
  ]

main :: IO Counts
main = do
  results <- runTestTT tests
  if errors results + failures results == 0
    then exitWith ExitSuccess
    else exitWith (ExitFailure 1)
