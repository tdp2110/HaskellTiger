module Color where

import qualified Assem
import qualified Graph
import qualified Liveness
import qualified X64Frame

import Control.Monad (when, forM_)
import Control.Monad.Loops (whileM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, runStateT, put, get)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
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
  , colors :: Map Int Int -- the color chosen by the algorithm for a node; for precolored
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
  , adjSet :: Set (Int, Int) -- set of interference edges in the graph
  , adjList :: Map Int (Set Int) -- adjacency list of graph: for each non-precolored
                                 -- temporary u, adjList[u] is the set of notes that
                                 -- interfere with u.
  , moveList :: Map Int [Int] -- mapping from node to the list of moves it is associated with
  }

data AllocatorReadOnlyData = AllocatorReadOnlyData {
    precolored :: Set Int -- machine registers, preassigned color
  , numColors :: Int
  , moveMap :: Map Int (Int, Int)
  }

type Allocator = StateT AllocatorState (
                   ReaderT AllocatorReadOnlyData Identity)

type Allocation = Map TempId X64Frame.Register

color :: Liveness.IGraph
      -> Allocation -- initial allocation, provided by Frame.tempMap
      -> (Int -> Float) -- spillCost
      -> [X64Frame.Register] -- available registers
      -> (Allocation, [Int]) -- assignments using available registers, list of spills
color = undefined

-- | sets up initial moveList, worklistMoves, coloredNodes, precolored
build :: Liveness.IGraph ->
         Set Int -> -- initial allocations
         ( Map Int [Int] -- moveList
         , Set Int -- worklistMoves
         , Set Int -- coloredNodes
         , Set Int -- precolored
         , Set Int -- initial
         )
build (Liveness.IGraph { Liveness.graph=graph
                       , Liveness.gtemp=gtemp }) initAlloc =
  let
    nodeList = Map.elems $ Graph.nodes graph
    lookups = fmap
                (\n -> let
                         temp = gtemp Map.! (Graph.nodeId n)
                       in
                         (Set.member temp initAlloc, temp, n)
                )
                nodeList
    nodesInInit = fmap
                    (\(_,t,n) -> (t,n))
                    $ filter
                        (\(isInTemp,_,_) -> isInTemp)
                        lookups
    precolored = fmap
                   (\(_,n) -> Graph.nodeId n)
                   nodesInInit
    colored = fmap
                (\(t,_) -> t)
                nodesInInit
    initial = fmap
                (\(_,_,n) -> Graph.nodeId n)
                $ filter
                    (\(isInTemp,_,_) -> not isInTemp)
                    lookups
  in
    undefined

addEdge :: Set Int -> Int -> Int -> Allocator ()
addEdge precolored' u v = do
  s1@AllocatorState {
         degree=degree'
       , adjSet=adjSet'
       , adjList=adjList'
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
          degree'' =
            Map.insert u ((degree' Map.! u) + 1) degree'
          in do
          put s1 { degree=degree''
                 , adjList=adjList'' }
      when (not (Set.member v precolored')) $
        let
          adjList'' =
            let adjList_v' = Set.insert u (adjList' Map.! v) in
              Map.insert v adjList_v' adjList'
          degree'' =
            Map.insert v ((degree' Map.! v) + 1) degree'
          in do
          s2 <- get
          put s2 { degree=degree''
                 , adjList=adjList'' }
      s3 <- get
      put s3 { adjSet=adjSet'' }

adjacent :: Int -> Allocator (Set Int)
adjacent n = do
  AllocatorState { selectStack=selectStack'
                 , coalescedNodes=coalescedNodes'
                 , adjList=adjList' } <- get
  pure $ (adjList' Map.! n) `Set.difference` (Set.fromList selectStack' `Set.union` coalescedNodes')

getMove :: Int -> Allocator (Maybe (Int, Int))
getMove m = do
  AllocatorReadOnlyData { moveMap=moveMap' } <- lift ask
  pure $ Map.lookup m moveMap'

nodeMoves :: Int -> Allocator (Set Int)
nodeMoves n = do
  AllocatorState { activeMoves=activeMoves'
                 , worklistMoves=worklistMoves'
                 , moveList=moveList' } <- get
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
coalesce = do
  st@AllocatorState { worklistMoves=worklistMoves'
                    , coalescedMoves=coalescedMoves'
                    , constrainedMoves=constrainedMoves'
                    , activeMoves=activeMoves'
                    , adjSet=adjSet' } <- get
  AllocatorReadOnlyData { moveMap=moveMap'
                        , precolored=precolored' } <- lift ask
  let
    m = Set.findMin worklistMoves'
    (x', y') = moveMap' Map.! m
    in do
    x <- getAlias x'
    y <- getAlias y'
    let
      (u,v) = if Set.member y precolored' then (y,x)
                                          else (x,y)
      worklistMoves'' = Set.delete m worklistMoves'
      coalescedMoves'' = Set.insert m coalescedMoves'
      constrainedMoves'' = Set.insert m constrainedMoves'
      activeMoves'' = Set.insert m activeMoves'
      in do
      adjacent_u <- adjacent u
      adjacent_v <- adjacent v
      isConservative <- conservative $ Set.toList $ adjacent_u `Set.union` adjacent_v
      adjacents_ok <- mapM (\t -> ok u t) $ Set.toList adjacent_v
      if u == v then do
        put st { worklistMoves=worklistMoves''
               , coalescedMoves=coalescedMoves'' }
        addWorkList u
      else if Set.member v precolored' || Set.member (u,v) adjSet' then do
        put st { worklistMoves=worklistMoves''
               , constrainedMoves=constrainedMoves'' }
        addWorkList u
        addWorkList v
      else if Set.member u precolored' && (all (==True) adjacents_ok)
                || not (Set.member u precolored') && isConservative then do
        put st { worklistMoves=worklistMoves''
               , coalescedMoves=coalescedMoves'' }
        combine u v
        addWorkList u
      else do
        put st { worklistMoves=worklistMoves''
               , activeMoves=activeMoves'' }

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
  AllocatorState { degree=degree'
                 , adjSet=adjSet' } <- get
  AllocatorReadOnlyData { numColors=numColors'
                        , precolored=precolored'
                        } <- lift ask
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

combine :: Int -> Int -> Allocator ()
combine u v = do
  s1@AllocatorState { freezeWorklist=freezeWorklist'
                    , spillWorklist=spillWorklist'
                    , coalescedNodes=coalescedNodes'
                    , alias=alias'
                    , degree=degree'
                    , moveList=moveList' } <- get
  AllocatorReadOnlyData { numColors=numColors'
                        , precolored=precolored' } <- lift ask
  adjacent_v <- adjacent v
  if Set.member v freezeWorklist' then
    put s1 { freezeWorklist=Set.delete v freezeWorklist' }
  else
    put s1 { spillWorklist=Set.delete v spillWorklist' }
  let
    coalescedNodes'' = Set.insert v coalescedNodes'
    alias'' = Map.insert v u alias'
    mv_u = moveList' Map.! u
    mv_v = moveList' Map.! v
    mv_u' = union mv_u mv_v
    moveList'' = Map.insert u mv_u' moveList'
    in do
    s2 <- get
    put s2 { coalescedNodes=coalescedNodes''
           , alias=alias''
           , moveList=moveList'' }
    forM_ adjacent_v (\t -> do
      addEdge precolored' t u
      decrementDegree t
      )
    when (degree' Map.! u >= numColors' &&
         Set.member u freezeWorklist') $ let
           freezeWorklist'' = Set.delete u freezeWorklist'
           spillWorklist'' = Set.insert u spillWorklist'
           in do
           s3 <- get
           put s3 { freezeWorklist=freezeWorklist''
                  , spillWorklist=spillWorklist'' }

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
freezeMoves u = do
  moves <- nodeMoves u
  mapM_ freezeMove moves
  where
    freezeMove m = do
      maybeMove <- getMove m
      case maybeMove of
        Just (x, y) -> do
          alias_x <- getAlias x
          alias_y <- getAlias y
          alias_u <- getAlias u
          st@AllocatorState { activeMoves=activeMoves'
                            , frozenMoves=frozenMoves'
                            , freezeWorklist=freezeWorklist'
                            , simplifyWorklist=simplifyWorklist'
                            , degree=degree' } <- get
          AllocatorReadOnlyData { numColors=numColors' } <- lift ask
          let
            v = if alias_x == alias_u then alias_x
                                      else alias_y
            activeMoves'' = Set.delete m activeMoves'
            frozenMoves'' = Set.insert m frozenMoves'
            in do
            nodeMoves_v <- nodeMoves v
            when (null nodeMoves_v && ((degree' Map.! v) < numColors')) $
              let
                freezeWorklist'' = Set.delete v freezeWorklist'
                simplifyWorklist'' = Set.insert v simplifyWorklist'
                in do
                put st { activeMoves=activeMoves''
                       , frozenMoves=frozenMoves''
                       , freezeWorklist=freezeWorklist''
                       , simplifyWorklist=simplifyWorklist'' }
        Nothing -> error "couldn't find a move when we expected one"

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
            st@AllocatorState { colors=colors' } <- get
            a <- getAlias n
            let
              colors'' = Map.insert a (colors' Map.! n) colors'
              in do
              put st { colors=colors'' })
   coalescedNodes'
 where
   stackNotEmpty = do
     AllocatorState { selectStack=selectStack' } <- get
     pure $ null selectStack'

   processStackElt = do
     st@AllocatorState { selectStack=selectStack'
                       , coloredNodes=coloredNodes'
                       , spilledNodes=spilledNodes'
                       , colors=colors'
                       , adjList=adjList' } <- get
     AllocatorReadOnlyData { precolored=precolored'
                           , numColors=numColors' } <- lift ask
     case selectStack' of
       (n:selectStack'') -> do
         adjacentAliases <- mapM getAlias $ Set.toList $ adjList' Map.! n
         let
           coloredAdjacentAliases = filter
                               (\a -> Set.member a $ coloredNodes' `Set.union` precolored')
                               adjacentAliases
           colorsAdjacent = fmap
                              (\a -> colors' Map.! a)
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
               colors'' = Map.insert n c colors'
               in do
               put st { selectStack=selectStack''
                      , coloredNodes=coloredNodes''
                      , colors=colors'' }

       [] -> error "shouldn't get here: stackNotEmpty shouldn't allow it"