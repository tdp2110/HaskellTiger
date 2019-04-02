module Main where

import Ch1
import Test.HUnit
import System.Exit

res1 = interpStm (CompoundStm (CompoundStm (AssignStm "var" $ NumExp 42) (AssignStm "x" prog1)) (PrintStm [IdExp "var", IdExp "x"])) []

res2 = interpStm (PrintStm [IdExp "var", OpExp (NumExp 1) Div (IdExp "var")]) [("var", 0)]

test1 :: Test
test1 = TestCase (let (str, _, _) = res1 in assertEqual "test1" str "42 1295\n")

tests :: Test
tests = TestList [TestLabel "test1" test1]

main :: IO Counts
main = do
  results <- runTestTT tests
  if (errors results + failures results == 0)
    then
    exitWith ExitSuccess
    else
    exitWith (ExitFailure 1)
