module Main where

import qualified Assem
import qualified Canon
import qualified Codegen
import qualified Liveness
import qualified Parser
import qualified Semant
import qualified Temp
import qualified TreeIR
import qualified X64Frame

import Control.Monad.Trans.State (runState)
import Data.List
import qualified Data.Map as Map
import System.Environment (getArgs)

printTemp :: X64Frame.X64 -> Int -> String
printTemp x64 reg =
  case Map.lookup reg $ X64Frame.tempMap x64 of
    Just s -> s
    Nothing -> "t" ++ show reg

compileToAsm :: String -> String
compileToAsm text =
  case Parser.parse text of
    Left err -> show err
    Right ast ->
      case Semant.transThunked ast of
        Left err -> show err
        Right (_, frags, gen, x64) ->
          let
            header = "\t.section    __TEXT,__text,regular,pure_instructions\n" ++
                     "\t.intel_syntax noprefix\n"

            emit :: X64Frame.Frag -> Temp.Generator -> (String, Temp.Generator)
            emit (X64Frame.PROC { X64Frame.body=bodyStm
                                , X64Frame.fragFrame=frame }) gen' =
              let
                (stmts, gen'') = Canon.linearize bodyStm gen'
                ((blocks, lab), gen3) = runState (Canon.basicBlocks stmts) gen''
                (stmts', gen4) = runState (Canon.traceSchedule blocks lab) gen3
                (insts, gen5) = foldl'
                                  step1
                                  ([], gen4)
                                  stmts'
                insts' = X64Frame.procEntryExit3 frame insts
                formatAsm :: Assem.Inst -> String
                formatAsm asm =
                  let
                    formatImpl :: String -> [Int] -> [Int] -> Maybe [Assem.Label] -> String
                    formatImpl assem dsts srcs jmps =
                      let
                        readInt :: Char -> Int
                        readInt c = read $ c : [] :: Int
                        getJmp :: Maybe [Assem.Label] -> Int -> String
                        getJmp (Just labs) idx =
                          Temp.name $ labs !! idx
                        getJmp _ _ = error "shouldn't get here"
                        go ('`':'d':i:rest) = (printTemp x64 $ dsts !! readInt i) ++ go rest
                        go ('`':'s':i:rest) = (printTemp x64 $ srcs !! readInt i) ++ go rest
                        go ('`':'j':i:rest) = (getJmp jmps $ readInt i) ++ go rest
                        go (c:rest) = c : go rest
                        go [] = []
                      in
                        go assem
                  in
                    case asm of
                      Assem.OPER { Assem.assem=assem
                                 , Assem.operDst=operDst
                                 , Assem.operSrc=operSrc
                                 , Assem.jump=jmps } -> formatImpl
                                                          assem
                                                          operDst
                                                          operSrc
                                                          jmps
                      Assem.LABEL { Assem.assem=assem } -> assem
                      Assem.MOVE { Assem.assem=assem
                                 , Assem.moveDst=moveDst
                                 , Assem.moveSrc=moveSrc } -> formatImpl
                                                                assem
                                                                [moveDst]
                                                                [moveSrc]
                                                                Nothing
              in
                (intercalate "\n" (fmap formatAsm insts'), gen5)
              where
                step1 :: ([Assem.Inst], Temp.Generator) -> TreeIR.Stm -> ([Assem.Inst], Temp.Generator)
                step1 (insts, g) stm =
                  let
                    (insts', g') = Codegen.codegen x64 g stm
                  in
                    (insts ++ insts', g')
            emit (X64Frame.STRING (lab, str)) g =
              ((Temp.name lab) ++ ":\n\t.asciz\t\"" ++ str ++ "\"\n", g)
            step2 :: (String, Temp.Generator) -> X64Frame.Frag -> (String, Temp.Generator)
            step2 (s, g) f =
              let
                (s', g') = emit f g
              in
                (s ++ s', g')
          in
            header ++ (fst $ foldl' step2 ([], gen) frags)

main :: IO ()
main = do
  args <- getArgs
  str <- readFile $ head args
  putStrLn $ compileToAsm str
