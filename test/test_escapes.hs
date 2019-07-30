module Main where

import qualified Absyn
import FindEscape
import qualified Parser

import Test.HUnit
import System.Exit


parse :: String -> Absyn.Exp
parse text =
  let
    (Right ast) = Parser.parse text
  in
    ast

text1 :: String
text1 = "let\n" ++
        "  function foo(x:int, y:int) : int = \n" ++
        "    let function bar(z: int) : int = \n" ++
        "          z + y \n" ++
        "    in bar(x) end\n" ++
        "  in foo(1, 2) \n" ++
        "end"

test_directions1 :: Test
test_directions1 = TestCase (
  let
    ast = parse text1
    escapes = findEscapes ast
  in do
    assertEqual "directions1" escapes [[LetDec 0, FunDec 0, FunParam 1]]
  )

test_escapes1 :: Test
test_escapes1 = TestCase (
  let
    unescapedAst = parse text1
    escapedAst = escapeExp unescapedAst
    pathToX = [LetDec 0, FunDec 0, FunParam 0]
    pathToY = [LetDec 0, FunDec 0, FunParam 1]
    pathToZ = [LetDec 0, FunDec 0, FunBody, LetDec 0, FunDec 0, FunParam 0]
  in do
    assertEqual "x starts unescaped" False (getEscape unescapedAst pathToX)
    assertEqual "y starts unescaped" False (getEscape unescapedAst pathToY)
    assertEqual "z starts unescaped" False (getEscape unescapedAst pathToZ)
    assertEqual "x remains unescaped" False (getEscape escapedAst pathToX)
    assertEqual "y becomes escaped" True (getEscape escapedAst pathToY)
  )

tests :: Test
tests = TestList [
  TestLabel "test_directions1" test_directions1,
  TestLabel "test_escapes1" test_escapes1
  ]

main :: IO Counts
main = do
  results <- runTestTT tests
  if (errors results + failures results == 0)
    then
    exitWith ExitSuccess
    else
    exitWith (ExitFailure 1)
