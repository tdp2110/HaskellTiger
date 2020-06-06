module Main where

import qualified Assem
import qualified AssemOptim
import qualified Canon
import qualified Codegen
import qualified Lexer
import qualified Parser
import qualified RegAlloc
import qualified Semant
import qualified Temp
import qualified Translate
import qualified TreeIR
import qualified X64Frame
import qualified Data.Text                     as T

import           Control.Monad.Trans.State      ( runState )
import           Data.Char                      ( digitToInt )
import           Data.List
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           System.Environment             ( getArgs )


parseToExp :: String -> (Translate.Exp, Semant.FragList)
parseToExp text =
  let parseResult = Parser.parse text
  in  case parseResult of
        Left err -> error $ show err
        Right ast ->
          let semantResult = Semant.transProg ast
          in  case semantResult of
                Left err -> error $ show err
                Right (Semant.ExpTy { Semant.exp = expr }, frags, _, _) ->
                  (expr, frags)

showFrag :: X64Frame.Frag -> IO ()
showFrag X64Frame.PROC { X64Frame.body = body } = do
  putStrLn ";; FRAG PROC:"
  print body
  putStrLn ";; END FRAG"
showFrag (X64Frame.STRING (lab, str)) = do
  putStrLn ";; FRAG STRING:\n"
  print (lab, str)
  putStrLn ";; END FRAG"

printTemp :: Map Int X64Frame.Register -> Int -> String
printTemp alloc temp = case Map.lookup temp alloc of
  Just s  -> show s
  Nothing -> "t" ++ show temp

formatAsm :: Map Int X64Frame.Register -> Assem.Inst -> String
formatAsm alloc asm =
  let
    formatImpl :: String -> [Int] -> [Int] -> Maybe [Assem.Label] -> String
    formatImpl assem dsts srcs jmps =
      let
        getJmp :: Maybe [Assem.Label] -> Int -> String
        getJmp (Just labs) idx = T.unpack . Temp.name $ labs !! idx
        getJmp _           _   = error "shouldn't get here"
        go ('`' : 'd' : i : rest) =
          printTemp alloc (dsts !! digitToInt i) ++ go rest
        go ('`' : 's' : i : rest) =
          printTemp alloc (srcs !! digitToInt i) ++ go rest
        go ('`' : 'j' : i : rest) = getJmp jmps (digitToInt i) ++ go rest
        go (c             : rest) = c : go rest
        go []                     = []
      in
        go assem
  in
    case asm of
      Assem.OPER { Assem.assem = assem, Assem.operDst = operDst, Assem.operSrc = operSrc, Assem.jump = jmps }
        -> formatImpl (T.unpack assem) operDst operSrc jmps
      Assem.LABEL { Assem.assem = assem } -> T.unpack assem
      Assem.MOVE { Assem.assem = assem, Assem.moveDst = moveDst, Assem.moveSrc = moveSrc }
        -> formatImpl (T.unpack assem) [moveDst] [moveSrc] Nothing
      Assem.STORECONST { Assem.assem = assem, Assem.strDst = strDst } ->
        formatImpl (T.unpack assem) [strDst] [] Nothing


compileToAsm :: String -> Bool -> String
compileToAsm text performRegAlloc = case Parser.parse text of
  Left  err -> show err
  Right ast -> case Semant.transThunked ast of
    Left err -> show err
    Right (_, frags, gen, x64) ->
      let
        header =
          "\t.globl _main\n"
            ++ "\t.section    __TEXT,__text,regular,pure_instructions\n"
            ++ "\t.intel_syntax noprefix\n"

        emit :: X64Frame.Frag -> Temp.Generator -> (String, Temp.Generator)
        emit X64Frame.PROC { X64Frame.body = bodyStm, X64Frame.fragFrame = frame } gen'
          = let
              (stmts, gen'') = Canon.linearize bodyStm gen'
              ((blocks, lab), gen3) = runState (Canon.basicBlocks stmts) gen''
              (stmts', gen4) = runState (Canon.traceSchedule blocks lab) gen3
              (insts, gen5) = foldl' step1 ([], gen4) stmts'
              insts' = X64Frame.procEntryExit2 frame insts
              maxCallArgs = TreeIR.maxCallArgsStm bodyStm
              tempMap = X64Frame.tempMap x64
              (insts'', (flowGraph, _)) = AssemOptim.optimize insts'
              (insts''', alloc, frame', gen6) = if performRegAlloc
                then RegAlloc.alloc insts'' flowGraph frame gen5 []
                else (insts'', tempMap, frame, gen5)
              insts4 = X64Frame.procEntryExit3
                frame'
                insts'''
                (X64Frame.MaxCallArgs maxCallArgs)
              insts5 = filter notEmptyInst insts4 -- an empty instr is appended to function bodies
                                                  -- in order to communicate some liveness info to regalloc
            in
              ( intercalate
                  "\n"
                  (fmap (formatAsm (alloc `Map.union` tempMap)) insts5)
                ++ "\n"
              , gen6
              )
         where
          notEmptyInst :: Assem.Inst -> Bool
          notEmptyInst Assem.OPER { Assem.assem = assem } = T.length assem /= 0
          notEmptyInst _ = True

          step1
            :: ([Assem.Inst], Temp.Generator)
            -> TreeIR.Stm
            -> ([Assem.Inst], Temp.Generator)
          step1 (insts, g) stm =
            let (insts', g') = Codegen.codegen x64 g stm
            in  (insts ++ insts', g')
        emit (X64Frame.STRING (lab, str)) g =
          ( T.unpack (Temp.name lab)
            ++ ":\n\t.asciz\t\""
            ++ T.unpack str
            ++ "\"\n"
          , g
          )
        step2
          :: (String, Temp.Generator)
          -> X64Frame.Frag
          -> (String, Temp.Generator)
        step2 (s, g) f = let (s', g') = emit f g in (s ++ s', g')
      in
        header ++ fst (foldl' step2 ([], gen) frags)

main :: IO ()
main = do
  args <- getArgs
  if "--lex" `elem` args
    then
      let args' = delete "--lex" args
      in  case Lexer.scanner $ head args' of
            Left  st -> error st
            Right ls -> print ls
    else if "--ast" `elem` args
      then
        let args' = delete "--ast" args
        in  do
              str <- readFile $ head args'
              print $ Parser.parse str
      else if "--treeIR" `elem` args
        then
          let args' = delete "--treeIR" args
          in  do
                str <- readFile $ head args'
                case parseToExp str of
                  (Translate.Ex treeExp, frags) -> do
                    mapM_ showFrag frags
                    print treeExp
                    pure ()
                  (Translate.Nx treeStm, frags) -> do
                    mapM_ showFrag frags
                    print treeStm
                    pure ()
                  (Translate.Cx _, frags) -> do
                    mapM_ showFrag frags
                    putStrLn "CX ..."
        else do
          let shouldRegAlloc = "--noreg" `notElem` args
          let args'          = delete "--noreg" args
          str <- readFile $ head args'
          putStrLn $ compileToAsm str shouldRegAlloc