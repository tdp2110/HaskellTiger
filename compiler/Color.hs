module Color where

import qualified Graph
import qualified Liveness as L
import qualified X64Frame

import Control.Monad (when, forM_)
import Control.Monad.Loops (whileM_, untilM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, runStateT, put, get)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Data.Function (on)
import Data.Functor.Identity
import Data.List
import Data.Map (Map)
import Data.Maybe (isJust)
import Data.Ord (comparing)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)


type TempId = L.TempId

type MoveSet = Set (L.NodeId, L.NodeId)
type WorkSet = Set L.NodeId

data AllocatorState = AllocatorState {
  {-
  * Node work-lists, sets, and stacks. *
  The following lists and sets are always mutually disjoint and
  covers all nodes.
  -}
    initial :: WorkSet -- temp registers, not precolored and not yet processed
  , simplifyWorklist :: WorkSet -- list of low-degree non-move-related nodes
  , freezeWorklist :: WorkSet -- low-degree move-related nodes
  , spillWorklist :: WorkSet -- high-degree nodes
  , spilledNodes :: WorkSet -- nodes marked for spilling during this round, initially empty
  , coalescedNodes :: WorkSet -- registers that have been coalesced, when u <- v coalesced,
                              -- v is added to this set and u put back on some work-list
  , coloredNodes :: WorkSet -- nodes sucessfully colored
  , selectStack :: [L.NodeId] -- stack containing temporaries removed from the graph
  , colors :: Map L.NodeId Int -- the color chosen by the algorithm for a node; for precolored
                               -- nodes this is initialized to the given color.
  {-
  *Move Sets*
  There are five sets of move instructions, and every move is
  in exactly one of these sets (after Build through the end of Main)
  -}
  , coalescedMoves :: MoveSet -- moves that have been coalesced.
  , constrainedMoves :: MoveSet -- moves whose source and target interfere
  , frozenMoves :: MoveSet -- moves that will no longer be considered for coalescing
  , worklistMoves :: MoveSet -- moves enabled for possible coalescing.
  , activeMoves :: MoveSet -- moves not yet ready for coalescing.
  , moveList :: Map L.NodeId [(L.NodeId, L.NodeId)] -- mapping from node to the list of moves it is associated with
  {-
  *Graph Stuff*
  -}
  , degree :: Map L.NodeId Int -- map containing the current degree of each node.
  , alias :: Map L.NodeId L.NodeId -- when a move (u, v) has been coalesced, and v is put
                                   -- in coalescedNodes, then alias(v) = u
  , adjSet :: MoveSet -- set of interference edges in the graph
  , adjList :: Map L.NodeId (Set L.NodeId) -- adjacency list of graph: for each non-precolored
                                 -- temporary u, adjList[u] is the set of nodes that
                                 -- interfere with u.
  }

data AllocatorReadOnlyData = AllocatorReadOnlyData {
    precolored :: Set L.NodeId -- machine registers, preassigned color
  , numColors :: Int
  , allColors :: [Int]
  }

type Allocator = StateT AllocatorState (
                   ReaderT AllocatorReadOnlyData Identity)

type Allocation = Map TempId X64Frame.Register

color :: L.IGraph ->
         Allocation -> -- initial allocation, provided by Frame.tempMap
         (Int -> Float) -> -- spillCost
         [X64Frame.Register] -> -- available registers
         (Allocation, [Int]) -- assignments using available registers, list of spills
color igraph@L.IGraph { L.gtemp=gtemp, L.tnode=tnode } initAlloc _ registers =
  let
    numColors' = length registers
    allColors' = [0 .. numColors' - 1]

    zippedRegColor = zip registers allColors'
    regToColor = Map.fromList zippedRegColor
    colorToReg = Map.fromList $ fmap swap zippedRegColor

    {-
    TODO Next
    problem is not all register temps appear in igraph tempMap.
    -}

    initialColors = Map.fromList $
      fmap
        (\(Just nodeId, reg) -> (nodeId, regToColor Map.! reg))
        $ filter
          (\(maybeNodeId, _) -> isJust maybeNodeId)
          $ fmap
              (\(tempId, reg) -> (fmap Graph.nodeId $ Map.lookup tempId tnode, reg))
              $ Map.toList initAlloc

    ( moveList'
      , worklistMoves'
      , coloredNodes'
      , precolored'
      , initial'' ) = build igraph initial'

    initialColoredNodes = Set.map
                            (\tempId -> Graph.nodeId $ tnode Map.! tempId)
                            coloredNodes'
    activeMoves' = Set.empty

    state = AllocatorState { initial=initial''
                           , simplifyWorklist=undefined
                           , freezeWorklist=undefined
                           , spillWorklist=undefined
                           , spilledNodes=Set.empty
                           , coalescedNodes=Set.empty
                           , coloredNodes=initialColoredNodes
                           , selectStack=[]
                           , colors=initialColors
                           , coalescedMoves=Set.empty
                           , constrainedMoves=Set.empty
                           , frozenMoves=Set.empty
                           , worklistMoves=worklistMoves'
                           , activeMoves=activeMoves'
                           , degree=Map.empty
                           , alias=Map.empty
                           , adjSet=Set.empty
                           , adjList=Map.empty
                           , moveList=moveList' }

    readOnlyData = AllocatorReadOnlyData { precolored=precolored'
                                         , numColors=numColors'
                                         , allColors=allColors' }

    -- fill in adjSet, adjList, degree by traversing igraph
    ((), state') = runIdentity $ runReaderT (runStateT buildGraph state) readOnlyData

    ( spillWorklist'
        , freezeWorklist'
        , simplifyWorklist' ) = makeWorkList
                                  (Set.toList initial'')
                                  numColors'
                                  activeMoves'
                                  worklistMoves'
                                  moveList'
                                  (degree state')

    state'' = state' { simplifyWorklist=simplifyWorklist'
                     , freezeWorklist=freezeWorklist'
                     , spillWorklist=spillWorklist' }
    ((), finalState) = runIdentity $ runReaderT (runStateT loop state'') readOnlyData

    finalAlloc = Map.fromList $
                   fmap
                     (\(nodeId, colorId) -> let
                                            tempId = gtemp Map.! nodeId
                                            reg = colorToReg Map.! colorId
                                          in
                                            (tempId, reg))
                     $ Map.toList $ colors finalState
    spills = fmap
               (\nodeId -> gtemp Map.! nodeId)
               $ Set.toList $ spilledNodes finalState
  in
    (finalAlloc, spills)
  where
    initial' :: Set TempId
    initial' =
      let
        graph = L.graph igraph
        nodeIds = fmap Graph.nodeId $ Map.elems $ Graph.nodes graph
        tempIds = fmap (\nodeId -> gtemp Map.! nodeId) nodeIds
        isNotInital = (\tempId -> case Map.lookup tempId initAlloc of
                                    Just _ -> False
                                    _      -> True)
      in
        Set.fromList $ filter isNotInital tempIds

    buildGraph :: Allocator ()
    buildGraph =
      let
        graph = L.graph igraph
      in do
        AllocatorReadOnlyData { precolored=precolored' } <- lift ask
        mapM_ (processNode precolored') $ Map.elems $ Graph.nodes graph

    processNode :: Set L.NodeId  -> L.Node -> Allocator ()
    processNode precolored' node =
      let
        nodeId = Graph.nodeId node
        successors = Graph.succ node
      in
        mapM_ (addEdge precolored' nodeId) successors

    loop :: Allocator ()
    loop = untilM_ loopAction loopDone

    loopAction :: Allocator ()
    loopAction = do
      AllocatorState { simplifyWorklist=simplifyWorklist'
                     , worklistMoves=worklistMoves'
                     , freezeWorklist=freezeWorklist'
                     , spillWorklist=spillWorklist' } <- get
      if      not $ null simplifyWorklist' then simplify
      else if not $ null worklistMoves'    then coalesce
      else if not $ null freezeWorklist'   then freeze
      else if not $ null spillWorklist'    then selectSpill
      else                                      pure ()

    loopDone :: Allocator Bool
    loopDone = do
      AllocatorState { simplifyWorklist=simplifyWorklist'
                     , worklistMoves=worklistMoves'
                     , freezeWorklist=freezeWorklist'
                     , spillWorklist=spillWorklist' } <- get
      pure $ null simplifyWorklist' &&
             null worklistMoves' &&
             null freezeWorklist' &&
             null spillWorklist'

-- | sets up initial moveList, worklistMoves, coloredNodes, precolored
build :: L.IGraph ->
         Set Int -> -- initial allocations
         ( Map L.NodeId [(L.NodeId, L.NodeId)] -- moveList
         , MoveSet -- worklistMoves
         , Set L.TempId -- coloredNodes
         , Set L.NodeId -- precolored
         , Set L.NodeId -- initial
         )
build (L.IGraph { L.graph=graph
                , L.gtemp=gtemp
                , L.moves=moves }) initAlloc =
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
    precoloredList = fmap
                   (\(_,n) -> Graph.nodeId n)
                   nodesInInit
    precolored' = Set.fromList precoloredList
    colored = fmap
                (\(t,_) -> t)
                nodesInInit
    initial' = fmap
                 (\(_,_,n) -> Graph.nodeId n)
                 $ filter
                     (\(isInTemp,_,_) -> not isInTemp)
                     lookups
    movePairs = fmap
                  (\(n1, n2) -> (Graph.nodeId n1, Graph.nodeId n2))
                  moves
    movesToAddSrcs = fmap
                       (\m@(src,_) -> (src, m))
                       $ filter
                           (\(src,_) -> Set.member src precolored')
                           movePairs
    movesToAddDsts = fmap
                       (\m@(_,dst) -> (dst, m))
                       $ filter
                           (\(_,dst) -> Set.member dst precolored')
                           movePairs
    movesToAdd = movesToAddSrcs ++ movesToAddDsts
    moveList' = Map.fromList $ groupByKey movesToAdd
    nodeIds = fmap Graph.nodeId nodeList
    nodesNoMoves = filter (\nodeId -> not $ Map.member nodeId moveList') nodeIds
    moveList'' = foldl'
                   (\acc nodeId -> Map.insert nodeId [] acc)
                   moveList'
                   nodesNoMoves
  in
    ( moveList''
    , Set.fromList movePairs
    , Set.fromList colored
    , precolored'
    , Set.fromList initial' )
  where
    groupByKey :: (Eq a, Ord a) => [(a, b)] -> [(a, [b])]
    groupByKey = map (\l -> (fst . head $ l, map snd l)) . groupBy ((==) `on` fst)
                 . sortBy (comparing fst)

makeWorkList :: [L.NodeId] ->
                Int ->
                MoveSet ->
                MoveSet ->
                Map L.NodeId [(L.NodeId, L.NodeId)] ->
                Map L.NodeId Int ->
                (WorkSet, WorkSet, WorkSet) -- spill, freeze, simplify
makeWorkList initials numColors' activeMoves' worklistMoves' moveList' degree' =
  let
    (spillWorklist', notHighDegree) = partition isHighDegree initials
    (freezeWorklist', simplifyWorklist') = partition moveRelated' notHighDegree
  in
    ( Set.fromList spillWorklist'
    , Set.fromList freezeWorklist'
    , Set.fromList simplifyWorklist' )
  where
    isHighDegree :: L.NodeId -> Bool
    isHighDegree n = case Map.lookup n degree' of
                       Just d -> d >= numColors'
                       Nothing -> False

    moveRelated' :: L.NodeId -> Bool
    moveRelated' n = moveRelated2
                       activeMoves'
                       worklistMoves'
                       moveList'
                       n

addEdge :: Set L.NodeId -> L.NodeId -> L.NodeId -> Allocator ()
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
            let adjList_u' = Set.insert v (Map.findWithDefault Set.empty u adjList') in
              Map.insert u adjList_u' adjList'
          oldDegree = Map.findWithDefault 0 u degree'
          degree'' =
            Map.insert u (oldDegree + 1) degree'
          in do
          put s1 { degree=degree''
                 , adjList=adjList'' }
      when (not (Set.member v precolored')) $
        let
          adjList'' =
            let adjList_v' = Set.insert u (Map.findWithDefault Set.empty v adjList') in
              Map.insert v adjList_v' adjList'
          oldDegree = Map.findWithDefault 0 v degree'
          degree'' =
            Map.insert v (oldDegree + 1) degree'
          in do
          s2 <- get
          put s2 { degree=degree''
                 , adjList=adjList'' }
      s3 <- get
      put s3 { adjSet=adjSet'' }

adjacent :: L.NodeId -> Allocator (Set L.NodeId)
adjacent n = do
  AllocatorState { selectStack=selectStack'
                 , coalescedNodes=coalescedNodes'
                 , adjList=adjList' } <- get
  pure $ (Map.findWithDefault Set.empty n adjList')
         `Set.difference`
         (Set.fromList selectStack' `Set.union` coalescedNodes')

nodeMoves :: L.NodeId -> Allocator (MoveSet)
nodeMoves n = do
  AllocatorState { activeMoves=activeMoves'
                 , worklistMoves=worklistMoves'
                 , moveList=moveList' } <- get
  pure $ nodeMoves2
           activeMoves'
           worklistMoves'
           moveList'
           n

nodeMoves2 :: MoveSet ->
              MoveSet ->
              Map L.NodeId [(L.NodeId, L.NodeId)] ->
              L.NodeId ->
              MoveSet
nodeMoves2 activeMoves' worklistMoves' moveList' n =
  (Set.fromList $ moveList' Map.! n)
         `Set.intersection`
         (activeMoves' `Set.union` worklistMoves')

moveRelated :: L.NodeId -> Allocator Bool
moveRelated n = do
  AllocatorState { activeMoves=activeMoves'
                 , worklistMoves=worklistMoves'
                 , moveList=moveList' } <- get
  pure $ moveRelated2
           activeMoves'
           worklistMoves'
           moveList'
           n

moveRelated2 :: MoveSet ->
                MoveSet ->
                Map L.NodeId [(L.NodeId, L.NodeId)] ->
                L.NodeId ->
                Bool
moveRelated2 activeMoves' worklistMoves' moveList' n =
  let
    moves = nodeMoves2
              activeMoves'
              worklistMoves'
              moveList'
              n
  in
    not $ Set.null moves

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

decrementDegree :: L.NodeId -> Allocator ()
decrementDegree m = do
  st@AllocatorState { degree=degree'
                    , spillWorklist=spillWorklist'
                    , freezeWorklist=freezeWorklist'
                    , simplifyWorklist=simplifyWorklist' } <- get
  isMoveRelated <- moveRelated m
  let
    d = Map.findWithDefault 0 m degree'
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

enableMoves :: [L.NodeId] -> Allocator ()
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
  AllocatorReadOnlyData { precolored=precolored' } <- lift ask
  let
    m@(x', y') = Set.findMin worklistMoves'
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

addWorkList :: L.NodeId -> Allocator ()
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

ok :: L.NodeId -> L.NodeId -> Allocator Bool
ok t r = do
  AllocatorState { degree=degree'
                 , adjSet=adjSet' } <- get
  AllocatorReadOnlyData { numColors=numColors'
                        , precolored=precolored'
                        } <- lift ask
  pure $ (degree' Map.! t < numColors') ||
         Set.member t precolored' ||
         Set.member (t, r) adjSet'

conservative :: [L.NodeId] -> Allocator Bool
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

getAlias :: L.NodeId -> Allocator L.NodeId
getAlias n = do
  AllocatorState { coalescedNodes=coalescedNodes'
                 , alias=alias' } <- get
  if Set.member n coalescedNodes' then
    getAlias $ alias' Map.! n
  else
    pure n

combine :: L.NodeId -> L.NodeId -> Allocator ()
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

freezeMoves :: L.NodeId -> Allocator ()
freezeMoves u = do
  moves <- nodeMoves u
  mapM_ freezeMove moves
  where
    freezeMove m@(x,y) = do
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
    chooseASpill :: Set L.NodeId -> Maybe L.NodeId
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
                           , allColors=allColors' } <- lift ask
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
           okColors = allColors' \\ colorsAdjacent
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
