module RegAlloc where

import qualified Assem
import qualified Codegen
import qualified Frame
import qualified Liveness
import qualified Temp
import qualified TreeIR
import qualified X64Frame

import Control.Monad (join, when)
import Control.Monad.Loops (whileM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (State, StateT, runStateT, runState, put, get)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Data.Foldable (foldl')
import Data.Functor.Identity
import Data.List
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
    initial :: Set Int -- temp registers, not precolored and not yet processed
  , simplifyWorklist :: Set Int -- list of low-degree non-move-related nodes
  , freezeWorklist :: Set Int -- low-degree move-related nodes
  , spillWorklist :: Set Int -- high-degree nodes
  , spilledNodes :: Set Int -- nodes marked for spilling during this round, initially empty
  , coalescedNodes :: Set Int -- registers that have been coalesced, when u <- v coalesced,
                              -- v is added to this set and u put back on some work-list
  , coloredNodes :: Set Int -- nodes sucessfully colored
  , selectStack :: [Int] -- stack containing temporaries removed from the graph
  , color :: Map Int Int -- the color chosen by the algorithm for a node; for precolored
                         -- nodes this is initialized to the given color.
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
  , degree :: Map Int Int -- map containing the current degree of each node.
  , alias :: Map Int Int -- when a move (u, v) has been coalesced, and v is put
                         -- in coalescedNodes, then alias(v) = u
  }

data AllocatorReadOnlyData = AllocatorReadOnlyData {
    adjSet :: Set (Int, Int) -- set of interference edges in the graph
  , precolored :: Set Int -- machine registers, preassigned color
  , adjList :: Map Int (Set Int) -- adjacency list of graph: for each non-precolored
                                 -- temporary u, adjList[u] is the set of notes that
                                 -- interfere with u.
  , moveList :: Map Int [Int] -- mapping from node to the list of moves it is associated with
  , numColors :: Int
  }

type Allocator = StateT AllocatorState (
                   ReaderT AllocatorReadOnlyData Identity)

type Allocation = Map TempId X64Frame.Register
alloc :: [Assem.Inst] -> X64Frame.X64Frame -> ([Assem.Inst], Allocation)
alloc = undefined

data ReadOnlyDataBuilder = ReadOnlyDataBuilder {
                               degrees :: Map Int Int
                             , allocatorData :: AllocatorReadOnlyData
                             }

addEdge :: Int -> Int -> State ReadOnlyDataBuilder ()
addEdge u v = do
  ReadOnlyDataBuilder {
         degrees=degrees'
       , allocatorData=AllocatorReadOnlyData {
             adjSet=adjSet'
           , precolored=precolored'
           , adjList=adjList'
       }
  } <- get
  when (not (Set.member (u,v) adjSet') && (u /= v)) $
    let
      adjSet'' = adjSet' `Set.union` Set.fromList [ (u,v), (v,u) ]
      in do
      when (not (Set.member u precolored')) $
        let
          adjList'' =
            let adjList_u' = Set.insert v (adjList' Map.! u) in
              Map.insert u adjList_u' adjList'
          degrees'' =
            Map.insert u ((degrees' Map.! u) + 1) degrees'
          in do
          state@ReadOnlyDataBuilder { allocatorData=allocatorData' } <- get
          put state { degrees=degrees''
                    , allocatorData=allocatorData' { adjList=adjList'' } }
      when (not (Set.member v precolored')) $
        let
          adjList'' =
            let adjList_v' = Set.insert u (adjList' Map.! v) in
              Map.insert v adjList_v' adjList'
          degrees'' =
            Map.insert v ((degrees' Map.! v) + 1) degrees'
          in do
          state@ReadOnlyDataBuilder { allocatorData=allocatorData' } <- get
          put state { degrees=degrees''
                    , allocatorData=allocatorData' { adjList=adjList'' } }
      state@ReadOnlyDataBuilder {
        allocatorData=allocatorData'@AllocatorReadOnlyData {}
        } <- get
      put state { allocatorData=allocatorData' { adjSet=adjSet'' } }

adjacent :: Int -> Allocator (Set Int)
adjacent n = do
  AllocatorState { selectStack=selectStack'
                 , coalescedNodes=coalescedNodes' } <- get
  AllocatorReadOnlyData { adjList=adjList' } <- lift ask
  pure $ (adjList' Map.! n) `Set.difference` (Set.fromList selectStack' `Set.union` coalescedNodes')

nodeMoves :: Int -> Allocator (Set Int)
nodeMoves n = do
  AllocatorState { activeMoves=activeMoves'
                 , worklistMoves=worklistMoves' } <- get
  AllocatorReadOnlyData { moveList=moveList' } <- lift ask
  pure $ (Set.fromList $ moveList' Map.! n) `Set.intersection` (activeMoves' `Set.union` worklistMoves')

moveRelated :: Int -> Allocator Bool
moveRelated n = do
  moves <- nodeMoves n
  pure $ not $ Set.null moves

simplify :: Allocator ()
simplify = do
  st@AllocatorState { simplifyWorklist=simplifyWorklist'
                    , selectStack=selectStack' } <- get
  if Set.null simplifyWorklist' then
     pure ()
  else
    let
      n = Set.findMin simplifyWorklist'
      simplifyWorklist'' = Set.delete n simplifyWorklist'
      selectStack'' = (n:selectStack')
      in do
        put st{ simplifyWorklist=simplifyWorklist''
              , selectStack=selectStack''}
        adjacents <- adjacent n
        mapM_ decrementDegree $ adjacents

decrementDegree :: Int -> Allocator ()
decrementDegree m = do
  st@AllocatorState { degree=degree'
                    , spillWorklist=spillWorklist'
                    , freezeWorklist=freezeWorklist'
                    , simplifyWorklist=simplifyWorklist' } <- get
  isMoveRelated <- moveRelated m
  let
    d = degree' Map.! m
    degree'' = Map.insert m (d - 1) degree'
    spillWorklist'' = Set.delete m spillWorklist'
    freezeWorklist'' = if isMoveRelated then
                         Set.insert m freezeWorklist'
                       else
                         freezeWorklist'
    simplifyWorklist'' = if isMoveRelated then
                           simplifyWorklist'
                         else
                           Set.insert m simplifyWorklist'
    in do
      put st { degree=degree''
             , spillWorklist=spillWorklist''
             , freezeWorklist=freezeWorklist''
             , simplifyWorklist=simplifyWorklist'' }
      pure ()

enableMoves :: [Int] -> Allocator ()
enableMoves nodes =
  mapM_ enableMoves2 nodes
  where
    enableMoves2 node = do
      moves <- nodeMoves node
      mapM_ enableMoves3 moves
    enableMoves3 m = do
      st@AllocatorState { activeMoves=activeMoves'
                        , worklistMoves=worklistMoves' } <- get
      when (Set.member m activeMoves') $ let
        activeMoves'' = Set.delete m activeMoves'
        worklistMoves'' = Set.insert m worklistMoves'
        in do
        put st { activeMoves=activeMoves''
               , worklistMoves=worklistMoves'' }

coalesce :: Allocator ()
coalesce = undefined

addWorkList :: Int -> Allocator ()
addWorkList u = do
  st@AllocatorState { freezeWorklist=freezeWorklist'
                    , simplifyWorklist=simplifyWorklist'
                    , degree=degree' } <- get
  AllocatorReadOnlyData { numColors=numColors'
                        , precolored=precolored' } <- lift ask
  isMoveRelated <- moveRelated u
  when ((Set.member u precolored') &&
        not isMoveRelated &&
        (degree' Map.! u < numColors')) $ let
    freezeWorklist'' = Set.delete u freezeWorklist'
    simplifyWorklist'' = Set.insert u simplifyWorklist'
    in do
    put st { freezeWorklist=freezeWorklist''
           , simplifyWorklist=simplifyWorklist'' }

ok :: Int -> Int -> Allocator Bool
ok t r = do
  AllocatorState {  degree=degree' } <- get
  AllocatorReadOnlyData { numColors=numColors'
                        , precolored=precolored'
                        , adjSet=adjSet' } <- lift ask
  pure $ (degree' Map.! t < numColors') ||
         Set.member t precolored' ||
         Set.member (t, r) adjSet'

conservative :: [Int] -> Allocator Bool
conservative nodes = do
  AllocatorState { degree=degree' } <- get
  AllocatorReadOnlyData { numColors=numColors' } <- lift ask
  let
    k = length $ filter (hasSignificantDegree degree' numColors') nodes
    in do
      pure $ k < numColors'
  where
    hasSignificantDegree degree' numColors' n =
      degree' Map.! n >= numColors'

getAlias :: Int -> Allocator Int
getAlias n = do
  AllocatorState { coalescedNodes=coalescedNodes'
                 , alias=alias' } <- get
  if Set.member n coalescedNodes' then
    getAlias $ alias' Map.! n
  else
    pure n

{-
combine :: Int -> Int -> Allocator ()
combine u v = do
  st@AllocatorState { freezeWorklist=freezeWorklist'
                    , spillWorklist=spillWorklist'
                    , coalescedNodes=coalescedNodes'
                    , alias=alias' } <- get
  if Set.member v freezeWorklist' then
    put st { freezeWorklist=Set.delete v freezeWorklist' }
  else
    put st { spillWorklist=Set.delete v spillWorklist' }
  let
    coalescedNodes'' = Set.delete v coalescedNodes'
    alias'' = Map.insert v u alias'
    -- TODO wtf?? p. 248
    -- nodeMoves[u] <- nodeMoves[u] \Union nodeMoves[v]
    in do
-}

freeze :: Allocator ()
freeze = do
  st@AllocatorState { freezeWorklist=freezeWorklist'
                    , simplifyWorklist=simplifyWorklist' } <- get
  case Set.lookupMin freezeWorklist' of
    Just u -> let
      freezeWorklist'' = Set.delete u freezeWorklist'
      simplifyWorklist'' = Set.insert u simplifyWorklist'
      in do
        put st { freezeWorklist=freezeWorklist''
               , simplifyWorklist=simplifyWorklist'' }
        freezeMoves u
    Nothing -> do pure ()

freezeMoves :: Int -> Allocator ()
freezeMoves = undefined

selectSpill :: Allocator ()
selectSpill = do
  st@AllocatorState { spillWorklist=spillWorklist'
                    , simplifyWorklist=simplifyWorklist' } <- get
  case chooseASpill spillWorklist' of
    Just m -> let
      spillWorklist'' = Set.delete m spillWorklist'
      simplifyWorklist'' = Set.insert m simplifyWorklist'
      in do
        put st { spillWorklist=spillWorklist''
               , simplifyWorklist=simplifyWorklist'' }
        freezeMoves m
    Nothing -> do pure ()
  where
    -- TODO! Note: avoid choosing nodes that are the tiny live ranges
    --       resulting from the fetches of previously spilled registers.
    chooseASpill :: Set Int -> Maybe Int
    chooseASpill = Set.lookupMin

assignColors :: Allocator ()
assignColors = do
 whileM_ stackNotEmpty processStackElt
 AllocatorState { coalescedNodes=coalescedNodes' } <- get
 mapM_
   (\n -> do
            st@AllocatorState { color=color' } <- get
            a <- getAlias n
            let
              color'' = Map.insert a (color' Map.! n) color'
              in do
              put st { color=color'' })
   coalescedNodes'
 where
   stackNotEmpty = do
     AllocatorState { selectStack=selectStack' } <- get
     pure $ null selectStack'

   processStackElt = do
     st@AllocatorState { selectStack=selectStack'
                       , coloredNodes=coloredNodes'
                       , spilledNodes=spilledNodes'
                       , color=color' } <- get
     AllocatorReadOnlyData { adjList=adjList'
                           , precolored=precolored'
                           , numColors=numColors' } <- lift ask
     case selectStack' of
       (n:selectStack'') -> do
         adjacentAliases <- mapM getAlias $ Set.toList $ adjList' Map.! n
         let
           coloredAdjacentAliases = filter
                               (\a -> Set.member a $ coloredNodes' `Set.union` precolored')
                               adjacentAliases
           colorsAdjacent = fmap
                              (\a -> color' Map.! a)
                              coloredAdjacentAliases
           allColors = [0 .. numColors']
           okColors = allColors \\ colorsAdjacent
           in
           case okColors of
             [] -> let
               spilledNodes'' = Set.insert n spilledNodes'
               in do
               put st { selectStack=selectStack''
                      , spilledNodes=spilledNodes'' }
             (c:_) -> let
               coloredNodes'' = Set.insert n coloredNodes'
               color'' = Map.insert n c color'
               in do
               put st { selectStack=selectStack''
                      , coloredNodes=coloredNodes''
                      , color=color'' }

       [] -> error "shouldn't get here: stackNotEmpty shouldn't allow it"

newtype NewTemps = NewTemps [TempId]

rewriteProgram :: [Assem.Inst]
               -> X64Frame.X64Frame
               -> Set Int -- spills
               -> Temp.Generator
               -> ([Assem.Inst], NewTemps, X64Frame.X64Frame, Temp.Generator)
rewriteProgram insts frame spills gen =
  let
    spillsList = Set.toList spills
    (accesses, (frame', gen')) =
       runState (mapM allocLocal spillsList) (frame, gen)
    (insts', newTemps, gen'') = foldl' spillTemp (insts, [], gen') $ zip spillsList accesses
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
