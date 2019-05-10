module Main where

import qualified Parser
import qualified Semant
import qualified Types
import qualified Env

import Test.HUnit
import System.Exit
import Data.Either
import Data.List


parseToSema :: Env.VEnv -> Env.TEnv -> String -> Either Semant.SemantError Semant.ExpTy
parseToSema venv tenv text = let (Right ast) = Parser.parse text in
                     Semant.transExp venv tenv ast

intLiteral :: Test
intLiteral = TestCase (
  let
    text = "1337"
    (Right Semant.ExpTy{Semant.exp=_, Semant.ty=ty}) = parseToSema Env.baseVEnv Env.baseTEnv text
 in do
    assertEqual "int literal" Types.INT ty
  )

intArith1 :: Test
intArith1 = TestCase (
  let
    text = "42 + 1337"
    (Right Semant.ExpTy{Semant.exp=_, Semant.ty=ty}) = parseToSema Env.baseVEnv Env.baseTEnv text
 in do
    assertEqual "int arith 1" Types.INT ty
  )

intArith2 :: Test
intArith2 = TestCase (
  let
    text = "42 * 1337"
    (Right Semant.ExpTy{Semant.exp=_, Semant.ty=ty}) = parseToSema Env.baseVEnv Env.baseTEnv text
 in do
    assertEqual "int arith 2" Types.INT ty
  )

intArith3 :: Test
intArith3 = TestCase (
  let
    text = "(1 + 2 - (3 / (4 - 5) - 6 *(7 + 8)))"
    (Right Semant.ExpTy{Semant.exp=_, Semant.ty=ty}) = parseToSema Env.baseVEnv Env.baseTEnv text
 in do
    assertEqual "int arith 3" Types.INT ty
  )

strLiteral :: Test
strLiteral = TestCase (
  let
    text = "\"hello world\""
    (Right Semant.ExpTy{Semant.exp=_, Semant.ty=ty}) = parseToSema Env.baseVEnv Env.baseTEnv text
 in do
    assertEqual "str literal" Types.STRING ty
  )

strPlusIntIsErr :: Test
strPlusIntIsErr = TestCase (
  let
    text = "\"hello world\" + 2"
    semaResult = parseToSema Env.baseVEnv Env.baseTEnv text
  in do
    assertBool "can't add strings and ints" $ isLeft semaResult
  )

substringCall1 :: Test
substringCall1 = TestCase (
  let
    text = "substring(\"hello world\", 0, 1)"
    (Right (Semant.ExpTy _ ty)) = parseToSema Env.baseVEnv Env.baseTEnv text
  in do
    assertEqual "substring returns string" Types.STRING ty
  )

substringCall2 :: Test
substringCall2 = TestCase (
  let
    text = "substring(\"hello world\", nil, 1337)"
    (Left(Semant.SemantError err _)) = parseToSema Env.baseVEnv Env.baseTEnv text
  in do
    assertBool "wrongly typed param" $ isInfixOf "parameter 1" err
  )

substringCall3 :: Test
substringCall3 = TestCase (
  let
    text = "substring(\"hello world\", 42, 1337, nil)"
    (Left(Semant.SemantError err _)) = parseToSema Env.baseVEnv Env.baseTEnv text
  in do
    assertBool "wrong number of arguments" $ isInfixOf "expects 2 parameters but was passed 3" err
  )

intUncallable :: Test
intUncallable = TestCase (
  let
    text = "let var x := 2 in x(3) end"
    (Left(Semant.SemantError err _)) = parseToSema Env.baseVEnv Env.baseTEnv text
  in do
    assertEqual "integers are not callable" "omg" err
  )

forVar1 :: Test
forVar1 = TestCase (
  let
    text = "for j:=0 to 10 do j:=j+1"
    (Left(Semant.SemantError err _)) = parseToSema Env.baseVEnv Env.baseTEnv text
  in do
    assertEqual "can't assign to forVar" "forVar assigned in forBody" err
  )

forVar2 :: Test
forVar2 = TestCase (
  let
    text = "for j:=0 to 10 do (let var j := 2 in j + 1 end)"
    (Left(Semant.SemantError err _)) = parseToSema Env.baseVEnv Env.baseTEnv text
  in do
    assertEqual "can't assign to forVar" "forVar assigned in forBody" err
  )

tests :: Test
tests = TestList [TestLabel "ints" intLiteral,
                  TestLabel "int arith 1" intArith1,
                  TestLabel "int arith 2" intArith2,
                  TestLabel "int arith 3" intArith3,
                  TestLabel "str literal" strLiteral,
                  TestLabel "str plus int" strPlusIntIsErr,
                  TestLabel "substring1" substringCall1,
                  TestLabel "forVar1" forVar1,
                  TestLabel "forVar2" forVar2,
                  TestLabel "substring2" substringCall2,
                  TestLabel "substring3" substringCall2 --,
                  --TestLabel "intUncallable" intUncallable
                 ]

main :: IO Counts
main = do
  results <- runTestTT tests
  if (errors results + failures results == 0)
    then
    exitWith ExitSuccess
    else
    exitWith (ExitFailure 1)
