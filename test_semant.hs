module Main where

import qualified Parser
import qualified Semant
import qualified Types
import qualified Env

import Test.HUnit
import System.Exit
import Data.Either


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

tests :: Test
tests = TestList [TestLabel "ints" intLiteral,
                  TestLabel "int arith 1" intArith1,
                  TestLabel "int arith 2" intArith2,
                  TestLabel "str literal" strLiteral,
                  TestLabel "str plus int" strPlusIntIsErr]

main :: IO Counts
main = do
  results <- runTestTT tests
  if (errors results + failures results == 0)
    then
    exitWith ExitSuccess
    else
    exitWith (ExitFailure 1)
