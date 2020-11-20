module Main where

import qualified Assem
import qualified AssemOptim
import qualified Canon
import qualified Codegen
import qualified Flow
import qualified Graph
import qualified IROptim
import qualified Lexer
import qualified LLVMSemant
import qualified LLVMTranslate
import qualified Parser
import qualified RegAlloc
import qualified Semant
import qualified Temp
import qualified Translate
import qualified TreeIR
import qualified X64Frame
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import qualified Data.DList                    as DList

import           LLVM.Pretty

import           Control.Monad.Trans.State      ( runState )
import           Data.Char                      ( digitToInt )
import           Data.List
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           System.Environment             ( getArgs )
import           System.Console.GetOpt
import           Text.Pretty.Simple


removeUnneededFrags :: DList.DList X64Frame.Frag -> [X64Frame.Frag]
removeUnneededFrags frags =
  let frags' = DList.toList frags in removeUnnededProcFrags frags'

removeUnnededProcFrags :: [X64Frame.Frag] -> [X64Frame.Frag]
removeUnnededProcFrags frags =
  let shouldKeep  = shouldKeepFrag <$> zip frags (split frags)
      fragsToKeep = fmap fst $ filter snd $ zip frags shouldKeep
  in  fragsToKeep
 where
  shouldKeepFrag :: (X64Frame.Frag, [X64Frame.Frag]) -> Bool
  shouldKeepFrag (X64Frame.STRING _, _) = True
  shouldKeepFrag (X64Frame.PROC { X64Frame.fragFrame = X64Frame.X64Frame { X64Frame.name = lab } }, otherFrags)
    = (T.unpack (Temp.name lab) == "_main")
      || any (fragUsesLabel lab) otherFrags

split :: [a] -> [[a]]
split = go []
 where
  go _  []       = [[]]
  go ys (x : xs) = (ys ++ xs) : go (x : ys) xs

fragUsesLabel :: Temp.Label -> X64Frame.Frag -> Bool
fragUsesLabel _   (X64Frame.STRING _)                    = False
fragUsesLabel lab X64Frame.PROC { X64Frame.body = body } = stmUsesLab body
 where
  stmUsesLab :: TreeIR.Stm -> Bool
  stmUsesLab (TreeIR.MOVE (dst, src)) = expUsesLab dst || expUsesLab src
  stmUsesLab (TreeIR.EXP  e         ) = expUsesLab e
  stmUsesLab (TreeIR.JUMP (e, labs) ) = expUsesLab e || lab `elem` labs
  stmUsesLab (TreeIR.CJUMP (_, e1, e2, lab1, lab2)) =
    expUsesLab e1 || expUsesLab e2 || lab == lab1 || lab == lab2
  stmUsesLab (TreeIR.SEQ   (stm1, stm2)) = stmUsesLab stm1 || stmUsesLab stm2
  stmUsesLab (TreeIR.LABEL (lab', _   )) = lab == lab'

  expUsesLab :: TreeIR.Exp -> Bool
  expUsesLab (TreeIR.CONST _            ) = False
  expUsesLab (TreeIR.NAME  lab'         ) = lab == lab'
  expUsesLab (TreeIR.TEMP  _            ) = False
  expUsesLab (TreeIR.BINOP (_, e1, e2)  ) = expUsesLab e1 || expUsesLab e2
  expUsesLab (TreeIR.MEM   e            ) = expUsesLab e
  expUsesLab (TreeIR.CALL  (e, es, _, _)) = expUsesLab e || any expUsesLab es
  expUsesLab (TreeIR.CALLNORETURN (e, es, _)) =
    expUsesLab e || any expUsesLab es
  expUsesLab (TreeIR.ESEQ (s, e)) = stmUsesLab s || expUsesLab e

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

dumpCFG :: String -> Bool -> Bool -> String
dumpCFG text performRegAlloc optimize = case Parser.parse text of
  Left  err -> show err
  Right ast -> case Semant.transThunked ast of
    Left err -> show err
    Right (_, fragsDList, gen, x64) ->
      let
        frags = if optimize
          then removeUnneededFrags fragsDList
          else DList.toList fragsDList
        emit :: X64Frame.Frag -> Temp.Generator -> (String, Temp.Generator)
        emit X64Frame.PROC { X64Frame.body = bodyStm, X64Frame.fragFrame = frame } gen'
          = let
              (stmts        , gen'') = Canon.linearize bodyStm gen'
              ((blocks, lab), gen3 ) = runState (Canon.basicBlocks stmts) gen''
              blocks'                = if optimize
                then fmap IROptim.optimizeBasicBlock blocks
                else blocks
              (stmts', gen4) = runState (Canon.traceSchedule blocks' lab) gen3
              (insts , gen5)            = foldl' step1 ([], gen4) stmts'
              insts'                    = X64Frame.procEntryExit2 frame insts
              tempMap                   = X64Frame.tempMap x64
              (insts'', (flowGraph, _)) = if optimize
                then AssemOptim.optimizePreRegAlloc insts'
                else (insts', Flow.instrsToGraph insts')
              (insts''', alloc, _, gen6) = if performRegAlloc
                then
                  let (instsAlloc, allocs, frameAlloc, _, genAlloc) =
                        RegAlloc.alloc insts'' flowGraph frame gen5 []
                      (instsAllocOpt, _) = if optimize
                        then AssemOptim.optimizePostRegAlloc instsAlloc
                        else (instsAlloc, undefined)
                  in  (instsAllocOpt, allocs, frameAlloc, genAlloc)
                else (insts'', tempMap, frame, gen5)
              (flowGraph', nodes) = Flow.instrsToGraph insts'''
            in
              (dumpFragCFG insts''' flowGraph' nodes alloc, gen6)
        emit (X64Frame.STRING _) g = ([], g)

        step1
          :: ([Assem.Inst], Temp.Generator)
          -> TreeIR.Stm
          -> ([Assem.Inst], Temp.Generator)
        step1 (insts, g) stm =
          let (insts', g') = Codegen.codegen x64 g stm in (insts ++ insts', g')

        dumpFragCFG insts Flow.FlowGraph { Flow.control = cfg } nodes tempMap =
          let
            nodeIdInstPairs = zip (fmap Graph.nodeId nodes) insts
            nodeIdToInst    = Map.fromList nodeIdInstPairs
            nodePrinter nodeId =
              let Just inst = Map.lookup nodeId nodeIdToInst
              in  formatAsm tempMap inst
          in
            Graph.toDot cfg nodePrinter

        processFrag
          :: (String, Temp.Generator)
          -> X64Frame.Frag
          -> (String, Temp.Generator)
        processFrag (s, g) f = let (s', g') = emit f g in (s ++ s', g')
      in
        fst $ foldl' processFrag ([], gen) frags

showFlatIR :: String -> Bool -> String
showFlatIR text optimize = case Parser.parse text of
  Left  err -> show err
  Right ast -> case Semant.transThunked ast of
    Left err -> show err
    Right (_, fragsDList, gen, _) ->
      let
        frags = if optimize
          then removeUnneededFrags fragsDList
          else DList.toList fragsDList
        emit :: X64Frame.Frag -> Temp.Generator -> (Canon.Block, Temp.Generator)
        emit X64Frame.PROC { X64Frame.body = bodyStm } gen' =
          let
            (stmts        , gen'') = Canon.linearize bodyStm gen'
            ((blocks, lab), gen3 ) = runState (Canon.basicBlocks stmts) gen''
            blocks'                = if optimize
              then fmap IROptim.optimizeBasicBlock blocks
              else blocks
            (stmts', gen4) = runState (Canon.traceSchedule blocks' lab) gen3
          in
            (stmts', gen4)
        emit (X64Frame.STRING _) g = ([], g)
        accum
          :: (Canon.Block, Temp.Generator)
          -> X64Frame.Frag
          -> (Canon.Block, Temp.Generator)
        accum (b, g) f = let (b', g') = emit f g in (b ++ b', g')
        flatIr = fst (foldl' accum ([] :: Canon.Block, gen) frags)
      in
        intercalate "\n" (fmap show flatIr)

compileToAsm :: String -> Bool -> Bool -> String
compileToAsm text performRegAlloc optimize = case Parser.parse text of
  Left  err -> show err
  Right ast -> case Semant.transThunked ast of
    Left err -> show err
    Right (_, fragsDList, gen, x64) ->
      let
        frags = if optimize
          then removeUnneededFrags fragsDList
          else DList.toList fragsDList
        header =
          "\t.globl _main\n"
            ++ "\t.section    __TEXT,__text,regular,pure_instructions\n"
            ++ "\t.intel_syntax noprefix\n"

        emit :: X64Frame.Frag -> Temp.Generator -> (String, Temp.Generator)
        emit X64Frame.PROC { X64Frame.body = bodyStm, X64Frame.fragFrame = frame } gen'
          = let
              (stmts        , gen'') = Canon.linearize bodyStm gen'
              ((blocks, lab), gen3 ) = runState (Canon.basicBlocks stmts) gen''
              blocks'                = if optimize
                then fmap IROptim.optimizeBasicBlock blocks
                else blocks
              (stmts', gen4) = runState (Canon.traceSchedule blocks' lab) gen3
              (insts, gen5) = foldl' step1 ([], gen4) stmts'
              insts' = X64Frame.procEntryExit2 frame insts
              maxCallArgs = TreeIR.maxCallArgsAndEscapesStm bodyStm
              tempMap = X64Frame.tempMap x64
              (insts'', (flowGraph, _)) = if optimize
                then AssemOptim.optimizePreRegAlloc insts'
                else (insts', Flow.instrsToGraph insts')
              (insts''', alloc, frame', gen6) = if performRegAlloc
                then
                  let (instsAlloc, allocs, frameAlloc, _, genAlloc) =
                        RegAlloc.alloc insts'' flowGraph frame gen5 []
                      (instsAllocOpt, _) = if optimize
                        then AssemOptim.optimizePostRegAlloc instsAlloc
                        else (instsAlloc, undefined)
                  in  (instsAllocOpt, allocs, frameAlloc, genAlloc)
                else (insts'', tempMap, frame, gen5)
              insts4 = X64Frame.procEntryExit3
                frame'
                insts'''
                (X64Frame.MaxCallArgsAndEscapes maxCallArgs)
              insts5 = filter notEmptyInst insts4 -- an empty instr is appended to function bodies
                                                  -- in order to communicate some liveness info to regalloc
              insts6 = AssemOptim.trimNoReturns insts5
            in
              ( intercalate
                  "\n"
                  (fmap (formatAsm (alloc `Map.union` tempMap)) insts6)
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

data Clopts = Clopts {
    optShowTokens :: Bool
  , optShowAst :: Bool
  , optShowTreeIR :: Bool
  , optNoRegAlloc :: Bool
  , optShowHelp :: Bool
  , optShowFlatIR :: Bool
  , optDumpCFG :: Bool
  , optO0 :: Bool
  , optEmitLLVM :: Bool
  } deriving Show

defaultClopts :: Clopts
defaultClopts = Clopts { optShowTokens = False
                       , optShowAst    = False
                       , optShowTreeIR = False
                       , optNoRegAlloc = False
                       , optShowHelp   = False
                       , optShowFlatIR = False
                       , optDumpCFG    = False
                       , optO0         = False
                       , optEmitLLVM   = False
                       }

options :: [OptDescr (Clopts -> Clopts)]
options =
  [ Option []
           ["show-tokens"]
           (NoArg (\opts -> opts { optShowTokens = True }))
           "tokenize input file"
  , Option []
           ["show-ast"]
           (NoArg (\opts -> opts { optShowAst = True }))
           "parse input file to AST"
  , Option []
           ["show-tree-ir"]
           (NoArg (\opts -> opts { optShowTreeIR = True }))
           "show tree intermediate representation"
  , Option []
           ["show-flat-ir"]
           (NoArg (\opts -> opts { optShowFlatIR = True }))
           "show flattened intermediate representation"
  , Option []
           ["dump-cfg"]
           (NoArg (\opts -> opts { optDumpCFG = True }))
           "dump dotgraph of control flow graphs (work in progress)"
  , Option []
           ["noreg"]
           (NoArg (\opts -> opts { optNoRegAlloc = True }))
           "compile to asm without performing register allocation"
  , Option []
           ["O0"]
           (NoArg (\opts -> opts { optO0 = True }))
           "optimization level 0 (no optimization)"
  , Option []
           ["emit-llvm"]
           (NoArg (\opts -> opts { optEmitLLVM = True }))
           "optimization level 0 (no optimization)"
  , Option ['h']
           ["help"]
           (NoArg (\opts -> opts { optShowHelp = True }))
           "show help"
  ]

parseClopts :: [String] -> IO (Clopts, [String])
parseClopts argv = case getOpt Permute options argv of
  (o, n, []  ) -> return (foldl (flip id) defaultClopts o, n)
  (_, _, errs) -> ioError (userError (concat errs ++ help))

help :: String
help = usageInfo "Usage: tigerc [OPTION...] files..." options

main :: IO ()
main = do
  args            <- getArgs
  (clopts, args') <- parseClopts args
  if optShowHelp clopts
    then putStrLn help
    else if optShowTokens clopts
      then do
        str <- readFile $ head args'
        case Lexer.scanner str of
          Left  err  -> error err
          Right toks -> print toks
      else if optShowAst clopts
        then do
          str <- readFile $ head args'
          case Parser.parse str of
            Left  err  -> error err
            Right expr -> pPrint expr
        else if optShowTreeIR clopts
          then do
            str <- readFile $ head args'
            case parseToExp str of
              (Translate.Ex treeExp, frags) -> do
                mapM_ showFrag frags
                print treeExp
              (Translate.Nx treeStm, frags) -> do
                mapM_ showFrag frags
                print treeStm
              (Translate.Cx _, frags) -> do
                mapM_ showFrag frags
                putStrLn "CX ..."
          else if optShowFlatIR clopts
            then do
              str <- readFile $ head args'
              putStrLn $ showFlatIR str (not $ optO0 clopts)
            else if optDumpCFG clopts
              then do
                str <- readFile $ head args'
                putStrLn $ dumpCFG str
                                   (not $ optNoRegAlloc clopts)
                                   (not $ optO0 clopts)
              else if optEmitLLVM clopts
                then do
                  str <- readFile $ head args'
                  case Parser.parse str of
                    Left  err  -> error err
                    Right expr -> do
                      let m    = LLVMTranslate.emptyModule "llvm-test"
                      let llvm = LLVMSemant.codegenTop expr
                      let m'   = LLVMTranslate.runLLVM m llvm
                      putStrLn $ LT.unpack $ ppllvm m'
                else do
                  str <- readFile $ head args'
                  putStrLn $ compileToAsm str
                                          (not $ optNoRegAlloc clopts)
                                          (not $ optO0 clopts)
