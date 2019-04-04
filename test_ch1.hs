module Main where

import Chapter1
import Test.HUnit
import System.Exit


test1 :: Test
test1 = TestCase (
  let (str, _, stopState) =  interpStm (
        CompoundStm (
            CompoundStm (AssignStm "var" $ NumExp 42) (AssignStm "x" prog1))
          (PrintStm [IdExp "var", IdExp "x"])) []
 in do
    assertEqual "test1 stdout" "42 1295\n" str
    assertEqual "test1 stop condition" stopState Ok
  )

test2 :: Test
test2 = TestCase (
  let (str, _, stopState) = interpStm (
        PrintStm [IdExp "var",
                  OpExp (NumExp 1) Div (IdExp "var")]) [("var", 0)] in do
    assertEqual "test2 stdout" "0\n" str
    assertEqual "test2 stop condition" (Fail DivByZero) stopState
  )

tests :: Test
tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]

main :: IO Counts
main = do
  results <- runTestTT tests
  if (errors results + failures results == 0)
    then
    exitWith ExitSuccess
    else
    exitWith (ExitFailure 1)
