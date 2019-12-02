module Liveness where

import qualified Graph as G
import qualified Flow as Flow

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import Control.Monad.Trans.State (StateT, runStateT, get, put)
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set


type TempId = Flow.TempId

newtype NodeId = NodeId Int

instance Eq NodeId where
  (NodeId n1) == (NodeId n2) = n1 == n2

instance Ord NodeId where
  (NodeId n1) `compare` (NodeId n2) = n1 `compare` n2

instance G.NodeId NodeId where
  incrId (NodeId nodeId) = NodeId $ nodeId + 1

type Graph = G.Graph NodeId
type GraphBuilder = G.GraphBuilder NodeId
type Node = G.Node NodeId

-- | graph: the interference graph;
--   tnode: a mapping from temporaries of the Assem program to graph nodes;
--   gtemp: inverse mapping, from graph nodes back to temporaries
--   moves: a list of move instructions. This is a hint to the register allocator;
--          if the "move instruction" (m, n) is on this list, it would be nice to
--          assign m and n the same register if possible.
data IGraph = IGraph { graph :: Graph
                     , tnode :: TempId -> Node
                     , gtemp :: Node -> TempId
                     , moves :: [(Node, Node)] }

interferenceGraph :: Flow.FlowGraph -> (IGraph, Flow.Node -> [TempId])
interferenceGraph flowGraph =
  let
    liveMap = buildLiveMap flowGraph
    _ = (runStateT . runWriterT . runWriterT) (buildGraph liveMap) $ G.newGraph $ NodeId 0
  in
    undefined
  where
    buildGraph :: Map Flow.NodeId (Set TempId) -> IGraphBuilder ()
    buildGraph liveMap = do
      tempsAndNodes <- allocNodes nodeIds
      let
        tempToNode = Map.fromList tempsAndNodes
        in
        mapM_ (processNode tempToNode) $ Map.toList liveMap

    allocNodes :: Set TempId -> IGraphBuilder [(TempId, Node)]
    allocNodes tempIds =
      mapM
        (\tempId -> do
                 node <- (lift . lift) G.allocNode
                 pure (tempId, node)
        )
        $ Set.toList tempIds

    nodeIds :: Set TempId
    nodeIds =
      let
        accumTemps :: Map Flow.NodeId [TempId] -> Set TempId
        accumTemps tempMap =
          foldl'
            (\acc tempIds -> Set.union acc $ Set.fromList tempIds)
            Set.empty
            $ Map.elems tempMap
        defs = accumTemps $ Flow.def flowGraph
        uses = accumTemps $ Flow.def flowGraph
      in
        Set.union defs uses

    processNode ::    Map TempId Node
                   -> (Flow.NodeId, Set TempId)
                   -> IGraphBuilder()
    processNode tempToNode (flowNode, liveSet) =
      let
        defs = (Flow.def flowGraph) Map.! flowNode
        --uses = (Flow.use flowGraph) Map.! flowNode
        isMove = (Flow.ismove flowGraph) Map.! flowNode
      in do
        mapM_ (addInterferenceEdges liveSet tempToNode isMove) defs

    addInterferenceEdges ::    Set TempId
                            -> Map TempId Node
                            -> Maybe (TempId, TempId) -- is the defined id a move?
                            -> TempId
                            -> IGraphBuilder ()
    addInterferenceEdges liveSet tempToNode isMove defdId =
      mapM_
        (\liveId -> let defdNode = tempToNode Map.! defdId
                        liveNode = tempToNode Map.! liveId
                        addEdgeAction = (lift . lift) $ G.addEdge defdNode liveNode
                    in case isMove of
                         Just (_, src) -> if liveId /= src then
                                              addEdgeAction
                                            else
                                              pure ()
                         Nothing -> addEdgeAction
                         )
        $ Set.toList liveSet

type Moves = [(Node, Node)]
type IGraphBuilder = WriterT Moves
                        (WriterT [(TempId, Node)] GraphBuilder)

-- | computes live-out sets per FlowGraph node
buildLiveMap :: Flow.FlowGraph -> Map Flow.NodeId (Set TempId)
buildLiveMap g =
  let
    flowKeys = Map.keys $ G.nodes $ Flow.control g
    liveIn = Map.fromList $
               fmap
                 (\nodeId -> (nodeId, Set.empty))
                 flowKeys
    liveOut = liveIn
  in
    buildImpl flowKeys liveIn liveOut
  where
    buildImpl :: [Flow.NodeId]
                 -> (Map Flow.NodeId (Set TempId))
                 -> (Map Flow.NodeId (Set TempId))
                 -> (Map Flow.NodeId (Set TempId))
    buildImpl flowKeys liveIn liveOut =
      let
        liveIn'  = Map.fromList $
                     fmap (updateIn liveOut) flowKeys
        liveOut' = Map.fromList $
                     fmap (updateOut liveIn) flowKeys
        shouldStop = liveIn' == liveIn && liveOut' == liveOut
      in
        if shouldStop then
          liveOut
        else
          buildImpl flowKeys liveIn' liveOut'

    updateIn :: Map Flow.NodeId (Set TempId)
                -> Flow.NodeId
                -> (Flow.NodeId, Set TempId)
    updateIn liveOut nodeId =
      -- in[n] = use[n] \union (out[n]-def[n])
      let
        usesList = (Flow.use g) Map.! nodeId
        usesSet = Set.fromList usesList
        out = liveOut Map.! nodeId
        defList = (Flow.def g) Map.! nodeId
        defSet = Set.fromList defList
      in
        ( nodeId
        , Set.union usesSet $ Set.difference out defSet )

    updateOut :: Map Flow.NodeId (Set TempId)
                -> Flow.NodeId
                -> (Flow.NodeId, Set TempId)
    updateOut liveIn nodeId =
      -- out[n] = \Union_{s \in succ[n]} in[s]
      let
        succs = G.succ $ (G.nodes (Flow.control g)) Map.! nodeId
        inOfSuccs = fmap
                      (\succId -> liveIn Map.! succId)
                      succs
        out' = foldl'
                 Set.union
                 Set.empty
                 inOfSuccs
      in
        (nodeId, out')
