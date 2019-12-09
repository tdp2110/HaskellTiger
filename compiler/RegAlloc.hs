module RegAlloc where

import qualified Assem
import qualified Flow
import qualified Graph
import qualified Liveness
import qualified Temp
import qualified X64Frame

import Control.Monad.Trans.State (State, runStateT)
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

data AllocatorState =
  AllocatorState {
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
