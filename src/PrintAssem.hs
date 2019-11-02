module Main where

import qualified Assem
import qualified Canon
import qualified Codegen
import qualified Parser
import qualified Semant
import qualified Temp
import qualified Tree
import qualified X64Frame

import Control.Monad (join)
import Control.Monad.Trans.State (runState)
import Data.List
import System.Environment (getArgs)
import Debug.Trace

debug = flip trace

compileToAsm :: String -> String
compileToAsm text =
  let
    (Right ast) = Parser.parse text
    (Right (Semant.ExpTy{Semant.exp=expr, Semant.ty=ty}, frags, gen, x64)) = Semant.transProg ast
    emit :: X64Frame.Frag -> Temp.Generator -> (String, Temp.Generator)
    emit (X64Frame.PROC{X64Frame.body=bodyStm}) gen' =
      let
        (stmts, gen'') = Canon.linearize bodyStm gen' `debug` (show bodyStm)
        ((blocks, lab), gen3) = runState (Canon.basicBlocks stmts) gen''
        (stmts', gen4) = runState (Canon.traceSchedule blocks lab) gen3
        (insts, gen5) = foldl'
                          step1
                          ([], gen4)
                          stmts'
      in
        ("PROC:\n" ++ join (map show insts), gen5)
      where
        step1 :: ([Assem.Inst], Temp.Generator) -> Tree.Stm -> ([Assem.Inst], Temp.Generator)
        step1 (insts, g) stm =
          let
            (insts', g') = Codegen.codegen x64 g stm
          in
            (insts ++ insts', g')
    emit (X64Frame.STRING (lab, str)) g =
      ("STRING: " ++ (show lab) ++ ":\n" ++ str ++ "\n", g)
    step2 :: (String, Temp.Generator) -> X64Frame.Frag -> (String, Temp.Generator)
    step2 (s, g) f =
      let
        (s', g') = emit f g
      in
        (s ++ s', g')
  in
    --fst $ foldl' step2 ([], gen) frags
    error $ show $ length frags

main :: IO ()
main = do
  args <- getArgs
  str <- readFile $ head args
  putStrLn $ compileToAsm str
