module RegAlloc where

import qualified Assem
import qualified Flow
import qualified Frame
import qualified Graph
import qualified Liveness
import qualified Temp
import qualified X64Frame

import Control.Monad (join)
import Control.Monad.Trans.State (State, runStateT, runState, put, get)
import Data.Foldable (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set


type TempId = Liveness.TempId

data NodeState =
    Precolored
  | Initial
  | SimplifyWork
  | FreezeWork
  | SpillWork
  | CoalescedNode
  | Colored
  | SelectStack
  deriving (Eq, Show)

data MoveState =
    CoalescedMove
  | Constrained
  | Frozen
  | WorklistMove
  | ActiveMove
  deriving (Eq, Show)

data AllocatorState = AllocatorState {
  {-
  * Node work-lists, sets, and stacks. *
  The following lists and sets are always mutually disjoint and
  covers all nodes.
  -}
    precolored :: Set Int -- machine registers, preassigned color
  , initial :: Set Int -- temp registers, not precolored and not yet processed
  , simplifyWorklist :: Set Int -- list of low-degree non-move-related nodes
  , freezeWorklist :: Set Int -- low-degree move-related nodes
  , spillWorklist :: Set Int -- high-degree nodes
  , spilledNodes :: Set Int -- nodes marked for spilling during this round, initially empty
  , coalescedNodes :: Set Int -- registers that have been coalesced, when u <- v coalesced,
                              -- v is added to this set and u put back on some work-list
  , coloredNodes :: Set Int -- nodes sucessfully colored
  , selectStack :: [Int] -- stack containing temporaries removed from the graph

  {-
  *Move Sets*
  There are five sets of move instructions, and every move is
  in exactly one of these sets (after Build through the end of Main)
  -}
  , coalescedMoves :: Set Int -- moves that have been coalesced.
  , constrainedMoves :: Set Int -- moves whose source and target interfere
  , frozenMoves :: Set Int -- moves that will no longer be considered for coalescing
  , worklistMoves :: Set Int -- moves enabled for possible coalescing.
  , activeMoves :: Set Int -- moves not yet ready for coalescing.
  }

type Allocation = Map TempId X64Frame.Register
alloc :: [Assem.Inst] -> X64Frame.X64Frame -> ([Assem.Inst], Allocation)
alloc = undefined

rewriteProgram :: [Assem.Inst]
               -> X64Frame.X64Frame
               -> Set Int -- spills
               -> Temp.Generator
               -> ([Assem.Inst], X64Frame.X64Frame, Temp.Generator)
rewriteProgram insts frame spills gen =
  let
    spillsList = Set.toList spills
    (accesses, (frame', gen')) =
       runState (mapM allocLocal spillsList) (frame, gen)
    (insts', gen'') = foldl' spillTemp (insts, gen') $ zip spillsList accesses
  in
    (insts', frame', gen')
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

    spillTemp :: ([Assem.Inst], Temp.Generator) -> (Int, X64Frame.X64Access)
              -> ([Assem.Inst], Temp.Generator)
    spillTemp (insts', gen') (tempId, X64Frame.InFrame k) =
      let
        storeInst :: Int -> Assem.Inst
        storeInst = undefined
        loadInst :: Int -> Assem.Inst
        loadInst = undefined

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

        rewriteInst :: Assem.Inst -> State Temp.Generator [Assem.Inst]
        rewriteInst inst = do
          maybeLoad <- if readsTemp inst then
                         do freshTemp <- newTemp
                            pure [loadInst freshTemp]
                       else
                         pure []
          maybeStore <- if writesTemp inst then
                         do freshTemp <- newTemp
                            pure [storeInst freshTemp]
                       else
                         pure []

          pure $ maybeLoad ++ [inst] ++ maybeStore

        (instsToJoin, gen'') = runState (mapM rewriteInst insts') gen'
      in
        (join instsToJoin, gen'')
