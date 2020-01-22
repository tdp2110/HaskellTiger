module RegAlloc where

import qualified Assem
import qualified Codegen
import qualified Color
import qualified Flow
import qualified Frame
import qualified Liveness
import qualified Temp
import qualified TreeIR
import qualified X64Frame

import Control.Monad (join)
import Control.Monad.Trans.State (State, runState, put, get)
import Data.Foldable (foldl')
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set


type TempId = Color.TempId

alloc :: [Assem.Inst]
      -> X64Frame.X64Frame
      -> Temp.Generator
      -> ( [Assem.Inst]
         , Color.Allocation
         , X64Frame.X64Frame
         , Temp.Generator )
alloc insts frame gen =
  let
    spillCost = \_ -> 1.0 :: Float -- TODO! use a better cost
    (flowGraph, _) = Flow.instrsToGraph insts
    (igraph, _) = Liveness.interferenceGraph flowGraph
    x64 = X64Frame.x64 frame
    initialAllocs = X64Frame.tempMap x64
    registers = X64Frame.registers x64
    (allocations, spills) = Color.color
                              igraph
                              initialAllocs
                              spillCost
                              registers
  in
    if null spills then
      ( filter (\i -> not $ isRedundant allocations i) insts
      , allocations
      , frame
      , gen)
    else
      let
        (insts', _, frame', gen') = rewriteProgram
                                      insts
                                      frame
                                      spills
                                      gen
      in
        alloc insts' frame' gen'
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
        _                                    -> False

newtype NewTemps = NewTemps [TempId]

rewriteProgram :: [Assem.Inst]
               -> X64Frame.X64Frame
               -> [Int] -- spills
               -> Temp.Generator
               -> ([Assem.Inst], NewTemps, X64Frame.X64Frame, Temp.Generator)
rewriteProgram insts frame spills gen =
  let
    (accesses, (frame', gen')) =
       runState (mapM allocLocal spills) (frame, gen)
    (insts', newTemps, gen'') =
      foldl' spillTemp (insts, [], gen') $ zip spills accesses
  in
    (insts', NewTemps newTemps, frame', gen'')
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

    spillTemp :: ([Assem.Inst], [TempId], Temp.Generator) -> (Int, X64Frame.X64Access)
              -> ([Assem.Inst], [TempId], Temp.Generator)
    spillTemp (insts', temps, gen') (tempId, frameAccess) =
      let
        accessExp = X64Frame.exp frameAccess $ TreeIR.TEMP $ Frame.fp frame

        storeCodeFn tempId' = do
          g <- get
          let
            storeStm = TreeIR.MOVE ( accessExp
                                   , TreeIR.TEMP tempId' )
            (code, g') = Codegen.codegen (X64Frame.x64 frame) g storeStm
            in do
            put g'
            pure code
        loadCodeFn tempId' = do
          g <- get
          let
            loadStm = TreeIR.MOVE ( TreeIR.TEMP tempId'
                                  , accessExp )
            (code, g') = Codegen.codegen (X64Frame.x64 frame) g loadStm
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

        rewriteInst :: Assem.Inst -> State Temp.Generator ([Assem.Inst], [TempId])
        rewriteInst inst = do
          (maybeLoad, maybeLoadTemp) <- if readsTemp inst then
                                          do freshTemp <- newTemp
                                             loadCode <- loadCodeFn freshTemp
                                             pure (loadCode, [freshTemp])
                                        else
                                          pure ([], [])
          (maybeStore, maybeStoreTemp) <- if writesTemp inst then
                                            do freshTemp <- newTemp
                                               storeCode <- storeCodeFn freshTemp
                                               pure (storeCode, [freshTemp])
                                          else
                                            pure ([], [])

          pure ( maybeLoad ++ [inst] ++ maybeStore
               , maybeLoadTemp ++ maybeStoreTemp)

        (toJoin, gen'') = runState (mapM rewriteInst insts') gen'
        insts'' = join $ fmap fst toJoin
        newTemps = join $ fmap snd toJoin
      in
        (insts'', temps ++ newTemps, gen'')
