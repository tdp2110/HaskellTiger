module RegAlloc where

import qualified Assem
import qualified Codegen
import qualified Color
import qualified Flow
import qualified Frame
import qualified Graph
import qualified Liveness
import qualified Temp
import qualified TreeIR
import qualified X64Frame

import Control.Monad (join)
import Control.Monad.Trans.State (State, runState, put, get)
import Data.Foldable (foldl')
import qualified Data.Map as Map

type TempId = Color.TempId


alloc :: [Assem.Inst]
      -> X64Frame.X64Frame
      -> Temp.Generator
      -> [Int]
      -> ( [Assem.Inst]
         , Color.Allocation
         , X64Frame.X64Frame
         , Temp.Generator )
alloc insts frame gen previousSpillTemps =
  let
    (flowGraph, _) = Flow.instrsToGraph insts
    (igraph, _) = Liveness.interferenceGraph flowGraph
    x64 = X64Frame.x64 frame
    initialAllocs = X64Frame.tempMap x64
    registers = X64Frame.registers x64
    (allocations, spills) = Color.color
                              igraph
                              initialAllocs
                              (spillCost flowGraph igraph)
                              registers
                              previousSpillTemps
  in
    if null spills then
      ( filter (\i -> not $ isRedundant allocations i) insts
      , allocations
      , frame
      , gen)
    else
      let
        (insts', newTemps, frame', gen') = rewriteProgram
                                             insts
                                             frame
                                             spills
                                             gen
      in
        alloc insts' frame' gen' newTemps
  where
    isRedundant :: Color.Allocation -> Assem.Inst -> Bool
    isRedundant allocation inst =
      case inst of
        Assem.MOVE { Assem.moveDst=moveDst
                   , Assem.moveSrc=moveSrc } ->
          case ( Map.lookup moveDst allocation
               , Map.lookup moveSrc allocation ) of
            (Just r1, Just r2) -> r1 == r2
            _                  -> False
        _                      -> False

    spillCost :: Flow.FlowGraph -> Liveness.IGraph -> Int -> Float
    spillCost (Flow.FlowGraph { Flow.control=control
                              , Flow.def=def
                              , Flow.use=use })
              (Liveness.IGraph { Liveness.tnode=tnode })
              temp =
      let
        useDefCount :: Float
        useDefCount = fromIntegral $ foldl'
                        (\acc nodeId ->
                           let
                             f = \s t -> fromEnum $ elem t s
                             defs = def Map.! nodeId
                             uses = use Map.! nodeId
                           in
                             acc + (f defs temp) + (f uses temp))
                        0
                        (Map.keys $ Graph.nodes control)
        nodeDegree :: Float
        nodeDegree = let
                       node = tnode Map.! temp
                     in
                       fromIntegral $ length $ Graph.succ node
      in
        useDefCount / nodeDegree

rewriteProgram :: [Assem.Inst]
               -> X64Frame.X64Frame
               -> [Int] -- spills
               -> Temp.Generator
               -> ([Assem.Inst], [Int], X64Frame.X64Frame, Temp.Generator)
rewriteProgram insts frame spills gen =
  let
    (accesses, (frame', gen')) =
       runState (mapM allocLocal spills) (frame, gen)
    (insts', newTemps, gen'') =
      foldl' (spillTemp frame') (insts, [], gen') $ zip spills accesses
  in
    (insts', newTemps, frame', gen'')
  where
    allocLocal _ = do
                     (frame', gen') <- get
                     let (gen'', frame'', access) = X64Frame.allocLocal
                                                      gen'
                                                      frame'
                                                      Frame.Escapes
                       in do
                       put (frame'', gen'')
                       pure access

    spillTemp :: X64Frame.X64Frame ->
                 ([Assem.Inst], [TempId], Temp.Generator) ->
                 (Int, X64Frame.X64Access) ->
                 ([Assem.Inst], [TempId], Temp.Generator)
    spillTemp frame' (insts', temps, gen') (tempId, frameAccess) =
      let
        accessExp = X64Frame.exp frameAccess $ TreeIR.TEMP $ Frame.fp frame'

        storeCodeFn tempId' = do
          g <- get
          let
            storeStm = TreeIR.MOVE ( accessExp
                                   , TreeIR.TEMP tempId' )
            (code, g') = Codegen.codegen (X64Frame.x64 frame') g storeStm
            in do
            put g'
            pure code
        loadCodeFn tempId' = do
          g <- get
          let
            loadStm = TreeIR.MOVE ( TreeIR.TEMP tempId'
                                  , accessExp )
            (code, g') = Codegen.codegen (X64Frame.x64 frame') g loadStm
            in do
            put g'
            pure code

        readsTemp :: Assem.Inst -> Bool
        readsTemp (Assem.OPER { Assem.operSrc=operSrc }) = elem tempId operSrc
        readsTemp (Assem.MOVE { Assem.moveSrc=moveSrc }) = tempId == moveSrc
        readsTemp _ = False

        writesTemp :: Assem.Inst -> Bool
        writesTemp (Assem.OPER { Assem.operDst=operDst }) = elem tempId operDst
        writesTemp (Assem.MOVE { Assem.moveDst=moveDst }) = tempId == moveDst
        writesTemp _ = False

        newTemp = do
          g <- get
          let
            (t, g') = Temp.newtemp g
            in
            do
              put g'
              pure t

        replace :: Eq a => a -> a -> a -> a
        replace toReplace replacer query = if query == toReplace then replacer
                                                                 else query

        replaceAll :: Eq a => a -> a -> [a] -> [a]
        replaceAll toReplace replacer = fmap $ replace toReplace replacer

        rewriteInst :: Assem.Inst -> State Temp.Generator ([Assem.Inst], [TempId])
        rewriteInst inst = do
          readTemp <- newTemp
          writeTemp <- newTemp
          (maybeLoad, maybeLoadTemp) <- if readsTemp inst then
                                          do loadCode <- loadCodeFn readTemp
                                             pure (loadCode, [readTemp])
                                        else
                                          pure ([], [])
          (maybeStore, maybeStoreTemp) <- if writesTemp inst then
                                            do storeCode <- storeCodeFn writeTemp
                                               pure (storeCode, [writeTemp])
                                          else
                                            pure ([], [])
          let
            inst' = case inst of
                      i@Assem.OPER { Assem.operSrc=operSrc } ->
                        i { Assem.operSrc=replaceAll tempId readTemp operSrc }
                      i@Assem.LABEL {} -> i
                      i@Assem.MOVE { Assem.moveSrc=moveSrc } ->
                        i { Assem.moveSrc=replace tempId readTemp moveSrc }

            inst'' = case inst' of
                       i@Assem.OPER { Assem.operDst=operDst } ->
                         i { Assem.operDst=replaceAll tempId writeTemp operDst }
                       i@Assem.LABEL {} -> i
                       i@Assem.MOVE { Assem.moveDst=moveDst } ->
                         i { Assem.moveDst = replace tempId writeTemp moveDst }
            in do
            pure ( maybeLoad ++ [inst''] ++ maybeStore
                 , maybeLoadTemp ++ maybeStoreTemp)

        (toJoin, gen'') = runState (mapM rewriteInst insts') gen'
        insts'' = join $ fmap fst toJoin
        newTemps = join $ fmap snd toJoin
      in
        (insts'', temps ++ newTemps, gen'')
