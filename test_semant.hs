module Main where

import qualified Parser
import qualified Semant
import qualified Types

import Test.HUnit
import System.Exit
import Data.Either
import Data.List


parseToSema :: String -> Either Semant.SemantError Semant.ExpTy
parseToSema text = let (Right ast) = Parser.parse text in
                     Semant.transProg ast

intLiteral :: Test
intLiteral = TestCase (
  let
    text = "1337"
    (Right Semant.ExpTy{Semant.exp=_, Semant.ty=ty}) = parseToSema text
 in do
    assertEqual "int literal" Types.INT ty
  )

intArith1 :: Test
intArith1 = TestCase (
  let
    text = "42 + 1337"
    (Right Semant.ExpTy{Semant.exp=_, Semant.ty=ty}) = parseToSema text
 in do
    assertEqual "int arith 1" Types.INT ty
  )

intArith2 :: Test
intArith2 = TestCase (
  let
    text = "42 * 1337"
    (Right Semant.ExpTy{Semant.exp=_, Semant.ty=ty}) = parseToSema text
 in do
    assertEqual "int arith 2" Types.INT ty
  )

intArith3 :: Test
intArith3 = TestCase (
  let
    text = "(1 + 2 - (3 / (4 - 5) - 6 *(7 + 8)))"
    (Right Semant.ExpTy{Semant.exp=_, Semant.ty=ty}) = parseToSema text
 in do
    assertEqual "int arith 3" Types.INT ty
  )

strLiteral :: Test
strLiteral = TestCase (
  let
    text = "\"hello world\""
    (Right Semant.ExpTy{Semant.exp=_, Semant.ty=ty}) = parseToSema text
 in do
    assertEqual "str literal" Types.STRING ty
  )

strPlusIntIsErr :: Test
strPlusIntIsErr = TestCase (
  let
    text = "\"hello world\" + 2"
    semaResult = parseToSema text
  in do
    assertBool "can't add strings and ints" $ isLeft semaResult
  )

substringCall1 :: Test
substringCall1 = TestCase (
  let
    text = "substring(\"hello world\", 0, 1)"
    (Right (Semant.ExpTy _ ty)) = parseToSema text
  in do
    assertEqual "substring returns string" Types.STRING ty
  )

substringCall2 :: Test
substringCall2 = TestCase (
  let
    text = "substring(\"hello world\", nil, 1337)"
    (Left(Semant.SemantError err _)) = parseToSema text
  in do
    assertBool "wrongly typed param" $ isInfixOf "parameter 1" err
  )

substringCall3 :: Test
substringCall3 = TestCase (
  let
    text = "substring(\"hello world\", 42, 1337, nil)"
    (Left(Semant.SemantError err _)) = parseToSema text
  in do
    assertBool "wrong number of arguments" $ isInfixOf "expects 2 parameters but was passed 3" err
  )

letExp1 :: Test
letExp1 = TestCase (
  let
    text = "let type Int = int\n" ++
           "    var n := 0\n" ++
           "    var m : Int := n + 1 in \n" ++
           "  let var k := 1 in \n" ++
           "    n := n + m;\n" ++
           "    if n = 0 then m - n + 2 * k\n" ++
           "    else n * (m + k)\n" ++
           "  end\n" ++
           "end"
    (Right Semant.ExpTy{Semant.exp=_, Semant.ty=ty}) = parseToSema text
  in do
    assertEqual "letExp example1" Types.INT ty
  )

intUncallable :: Test
intUncallable = TestCase (
  let
    text = "let var x := 2 in x(3) end"
    (Left(Semant.SemantError err _)) = parseToSema text
  in do
    assertEqual "integers are not callable" "only functions are callable: found type INT" err
  )

forVar1 :: Test
forVar1 = TestCase (
  let
    text = "for j:=0 to 10 do j:=j+1"
    (Left(Semant.SemantError err _)) = parseToSema text
  in do
    assertEqual "can't assign to forVar" "forVar assigned in forBody" err
  )

forVar2 :: Test
forVar2 = TestCase (
  let
    text = "for j:=0 to 10 do (let var k := 2 in j := j + k end)"
    (Left(Semant.SemantError err _)) = parseToSema text
  in do
    assertEqual "can't assign to forVar" "forVar assigned in forBody" err
  )

break1 :: Test
break1 = TestCase (
  let
    text = "for j:=0 to 10 do if j = 5 then break"
    (Right Semant.ExpTy{Semant.exp=_, Semant.ty=ty}) = parseToSema text
  in do
    assertEqual "break in forExp is ok" Types.UNIT ty
  )

nilRecord :: Test
nilRecord = TestCase (
  let
    text = "let\n" ++
           "  type intPair = { fst: int, snd: int }\n" ++
           "  var nilPair : intPair := nil\n" ++
           "in nilPair end"
    (Right Semant.ExpTy{Semant.exp=_, Semant.ty=ty}) = parseToSema text
  in do
    assertBool "nil record has record type" $ isRecord ty
  )

isRecord :: Types.Ty -> Bool
isRecord (Types.RECORD _) = True
isRecord _ = False

listTy1 :: Test
listTy1 = TestCase (
  let
    text = "let\n" ++
           "  type intList = { head: int, tail: intList}\n" ++
           "  var nilIntList : intList := nil\n" ++
           "  var xs := intList{head = 0, tail = nilIntList}\n" ++
           "in xs end"
    (Right Semant.ExpTy{Semant.exp=_, Semant.ty=ty}) = parseToSema text
  in do
    assertBool "instance of mutrec record has record ty" $ isRecord ty
  )

similarRecordDecs :: Test
similarRecordDecs = TestCase (
  let
    text = "let\n" ++
           "  type pair1 = { fst: int, snd: int }\n" ++
           "  type pair2 = { fst: int, snd: int }\n" ++
           "  var p1:= pair1{ fst=1, snd=2 }\n" ++
           "  var p2:= pair2{ fst=3, snd=4 }\n" ++
           "in p1.fst + p2.snd end"
    (Right Semant.ExpTy{Semant.exp=_, Semant.ty=ty}) = parseToSema text
  in do
    assertEqual "similar records" Types.INT ty
  )

arrayOfTypeAlias :: Test
arrayOfTypeAlias = TestCase (
  let
    text = "let\n" ++
           "  var N:= 16\n" ++
           "  type intAlias = int\n" ++
           "  type intArray = array of int\n" ++
           "  var xs := intArray [N] of 0\n" ++
           "in xs[8] end"
    (Right Semant.ExpTy{Semant.exp=_, Semant.ty=ty}) = parseToSema text
  in do
    assertEqual "similar records" Types.INT ty
  )

break2 :: Test
break2 = TestCase (
  let
    text = "if 1 then break"
    (Left(Semant.SemantError err _)) = parseToSema text
  in do
    assertEqual "can't assign to forVar" "break expression not enclosed in a while or for" err
  )

illegalDecls1 :: Test
illegalDecls1 = TestCase (
  let
    text = "let \n" ++
           "  var N:= 0 \n" ++
           "  function N() = nil \n" ++
           "in N() end"
    (Left(Semant.SemantError err _)) = parseToSema text
  in do
    assertBool "multiple declarations 1" $
      isInfixOf "multiple function or value declarations of symbol N in letExp declarations" err
  )

illegalDecls2 :: Test
illegalDecls2 = TestCase (
  let
    text = "let \n" ++
           "  var N:= 0 \n" ++
           "  var N := 42 \n" ++
           "in N + 1 end"
    (Left(Semant.SemantError err _)) = parseToSema text
  in do
    assertBool "multiple declarations 1" $
      isInfixOf "multiple function or value declarations of symbol N in letExp declarations" err
  )

illegalDecls3 :: Test
illegalDecls3 = TestCase (
  let
    text = "let \n" ++
           "  var N: string:= 0 \n" ++
           "in N + 1 end"
    (Left(Semant.SemantError err _)) = parseToSema text
  in do
    assertEqual "annotation error" "mismatch in type annotation and computed type in varDecl: type annotation STRING, computed type INT" err
  )

illegalDecls4 :: Test
illegalDecls4 = TestCase (
  let
    text = "let \n" ++
           "  var x:= nil \n" ++
           "in x end"
    (Left(Semant.SemantError err _)) = parseToSema text
  in do
    assertEqual "nil decls need record annotation 1" "nil expression declarations must be constrained by a RECORD type" err
  )

illegalDecls5 :: Test
illegalDecls5 = TestCase (
  let
    text = "let \n" ++
           "  var x:int:= nil \n" ++
           "in x end"
    (Left(Semant.SemantError err _)) = parseToSema text
  in do
    assertEqual "nil decls need record annotation 2" "nil expression declarations must be constrained by a RECORD type" err
  )

illegalDecls6 :: Test
illegalDecls6 = TestCase (
  let
    text = "let\n" ++
           "  var N := 8\n" ++
           "  type notAnArrayTy = int\n" ++
           "  var arr := notAnArrayTy [ N ] of 0" ++
           " in arr[0] end"
    (Left(Semant.SemantError err _)) = parseToSema text
  in do
    assertEqual "array decls must have an array type" "only array types may appear as the symbol in an array instance definition. Found type=INT" err
  )

illegalDecls7 :: Test
illegalDecls7 = TestCase (
  let
    text = "let\n" ++
           "  var N := 8\n" ++
           "  type intArray = array of int\n" ++
           "  var arr := intArray [ N ] of \"hello\"" ++
           " in arr[0] end"
    (Left(Semant.SemantError err _)) = parseToSema text
  in do
    assertEqual "array init expr must have array elt type" "in ArrayExp, initExp has actual type STRING, when it must have INT" err
  )

illegalDecls8 :: Test
illegalDecls8 = TestCase (
  let
    text = "let\n" ++
           "  type a = b\n" ++
           "  type b = c\n" ++
           "  type c = d\n" ++
           "  type d = a\n" ++
           "in 0 end"
    (Left(Semant.SemantError err _)) = parseToSema text
  in do
    assertBool "illegal cyclic type decl" $
      isInfixOf "found illegal type declaration cycle" err
  )

tests :: Test
tests = TestList [TestLabel "ints" intLiteral,
                  TestLabel "int arith 1" intArith1,
                  TestLabel "int arith 2" intArith2,
                  TestLabel "int arith 3" intArith3,
                  TestLabel "str literal" strLiteral,
                  TestLabel "str plus int" strPlusIntIsErr,
                  TestLabel "substring1" substringCall1,
                  TestLabel "nilRecord" nilRecord,
                  TestLabel "listTy1" listTy1,
                  TestLabel "similarRecordDecs" similarRecordDecs,
                  TestLabel "break1" break1,
                  TestLabel "break2" break2,
                  TestLabel "forVar1" forVar1,
                  TestLabel "forVar2" forVar2,
                  TestLabel "illegalDecls1" illegalDecls1,
                  TestLabel "illegalDecls2" illegalDecls2,
                  TestLabel "illegalDecls3" illegalDecls3,
                  TestLabel "illegalDecls4" illegalDecls4,
                  TestLabel "illegalDecls5" illegalDecls5,
                  TestLabel "illegalDecls6" illegalDecls6,
                  TestLabel "illegalDecls7" illegalDecls7,
                  TestLabel "illegalDecls8" illegalDecls8,
                  TestLabel "substring2" substringCall2,
                  TestLabel "substring3" substringCall2,
                  TestLabel "letExp1" letExp1,
                  TestLabel "arrayOfTypeAlias" arrayOfTypeAlias,
                  TestLabel "intUncallable" intUncallable
                 ]

main :: IO Counts
main = do
  results <- runTestTT tests
  if (errors results + failures results == 0)
    then
    exitWith ExitSuccess
    else
    exitWith (ExitFailure 1)
