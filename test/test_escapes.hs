module Main where

import qualified Absyn
import FindEscape
import qualified Parser
import Symbol

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
    assertEqual "directions1" [ (Symbol "y", [LetDec 0, FunDec 0])] escapes
  )

test_escapes1 :: Test
test_escapes1 = TestCase (
  let
    unescapedAst = parse text1
    escapedAst = escapeExp unescapedAst
    pathToX = (Symbol "x", [LetDec 0, FunDec 0])
    pathToY = (Symbol "y", [LetDec 0, FunDec 0])
    pathToZ = (Symbol "z", [LetDec 0, FunDec 0, FunBody, LetDec 0, FunDec 0])
  in do
    assertEqual "x starts unescaped" False $ getEscape unescapedAst pathToX
    assertEqual "y starts unescaped" False $ getEscape unescapedAst pathToY
    assertEqual "z starts unescaped" False $ getEscape unescapedAst pathToZ
    assertEqual "x remains unescaped" False $ getEscape escapedAst pathToX
    assertEqual "y becomes escaped" True $ getEscape escapedAst pathToY
    assertEqual "z remains unescaped" False $ getEscape unescapedAst pathToZ
  )

text2 :: String
text2 = "let\n" ++
        "  var N := 11\n" ++
        "  var K := 12\n" ++
        "  function foo(x:int, y:int) : int = \n" ++
        "    let\n" ++
        "      var M := 13\n"++
        "      var L := 14\n"++
        "      function bar(z: int) : int = \n" ++
        "          z + y + N + M\n" ++
        "    in bar(x) end\n" ++
        "  in foo(1, 2) \n" ++
        "end"

test_directions2 :: Test
test_directions2 = TestCase (
  let
    ast = parse text2
    escapes = findEscapes ast
  in do
    assertEqual "directions1" [(Symbol "y", [LetDec 2,FunDec 0]),
                               (Symbol "N",[]),
                               (Symbol "M",[LetDec 2,FunDec 0,FunBody])]
      escapes
  )

test_escapes2 :: Test
test_escapes2 = TestCase (
  let
    unescapedAst = parse text2
    escapedAst = escapeExp unescapedAst
    pathToN = (Symbol "N", [])
    pathToK = (Symbol "K", [])
    pathToX = (Symbol "x", [LetDec 2,FunDec 0])
    pathToY = (Symbol "y", [LetDec 2,FunDec 0])
    pathToM = (Symbol "M", [LetDec 2,FunDec 0,FunBody,LetDec 0])
    pathToL = (Symbol "L", [LetDec 2,FunDec 0,FunBody,LetDec 1])
    pathToZ = (Symbol "z", [LetDec 2,FunDec 0,FunBody,LetDec 2,FunDec 0])
  in do
    assertEqual "N starts unescaped" False $ getEscape unescapedAst pathToN
    assertEqual "K starts unescaped" False $ getEscape unescapedAst pathToK
    assertEqual "x starts unescaped" False $ getEscape unescapedAst pathToX
    assertEqual "y starts unescaped" False $ getEscape unescapedAst pathToY
    assertEqual "M starts unescaped" False $ getEscape unescapedAst pathToM
    assertEqual "L starts unescaped" False $ getEscape unescapedAst pathToL
    assertEqual "Z starts unescaped" False $ getEscape unescapedAst pathToZ

    assertEqual "N becomes escaped" True $ getEscape escapedAst pathToN
    assertEqual "K remains unescaped" False $ getEscape escapedAst pathToK
    assertEqual "x remains unescaped" False $ getEscape unescapedAst pathToX
    assertEqual "y becomes escaped" True $ getEscape escapedAst pathToY
    assertEqual "M becomes escaped" True $ getEscape escapedAst pathToM
    assertEqual "L remains unescaped" False $ getEscape escapedAst pathToL
    assertEqual "Z remains unescaped" False $ getEscape escapedAst pathToZ
  )

test_subscript :: Test
test_subscript = TestCase (
  let
    text = "let\n" ++
           "  var N := 7\n" ++
           "  type intArray = array of int\n" ++
           "  var arr := intArray[N] of 0\n" ++
           "  function foo(x: int) : int = \n" ++
           "    x + arr[N - 1]\n" ++
           "in\n" ++
           "  foo(2)\n" ++
           "end"
    escapes = findEscapes $ parse text
  in do
    assertEqual "subscript" [ (Symbol "N", [])
                            , (Symbol "arr", []) ]
        escapes
  )

test_assign :: Test
test_assign = TestCase (
  let
    text = "let\n" ++
           "    var x := 42\n" ++
           "    var y := 1337\n" ++
           "    function set_x(arg : int) =\n" ++
           "        x := arg\n" ++
           "in\n" ++
           "    println(itoa(x));\n" ++
           "    set_x(y);\n" ++
           "    println(itoa(x))\n" ++
           "end"
    escapes = findEscapes $ parse text
  in do
    assertEqual "assign" [(Symbol "x", [])] escapes
  )

test_recursiveTypes :: Test
test_recursiveTypes = TestCase (
  let
    text = "let\n" ++
           "    type IntList = { head: int, tail: IntList }\n" ++
           "    var nilIntList : IntList := nil\n" ++
           "    function sum(xs : IntList) : int =\n" ++
           "        if xs = nilIntList then\n" ++
           "                42          else\n" ++
           "                xs.head + sum(xs.tail)\n" ++
           "    var xs := IntList { head=0\n" ++
           "                      , tail=IntList { head=1\n" ++
           "                                     , tail=IntList { head=2\n" ++
           "                                                    , tail=nilIntList } } }\n" ++
           "in\n" ++
           "    println(itoa(sum(xs)))\n" ++
           "end"
    escapes = findEscapes $ parse text
  in do
    assertEqual "recursive" [(Symbol "nilIntList", [])] escapes
  )

tests :: Test
tests = TestList [ TestLabel "test_directions1" test_directions1
                 , TestLabel "test_escapes1" test_escapes1
                 , TestLabel "test_directions2" test_directions2
                 , TestLabel "test_escapes2" test_escapes2
                 , TestLabel "test_subscript" test_subscript
                 , TestLabel "test_assign" test_assign
                 , TestLabel "test_recursive" test_recursiveTypes
                 ]

main :: IO Counts
main = do
  results <- runTestTT tests
  if (errors results + failures results == 0)
    then
    exitWith ExitSuccess
    else
    exitWith (ExitFailure 1)
