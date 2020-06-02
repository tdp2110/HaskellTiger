module Liveness
  ( Node
  , TempId
  , NodeId(..)
  , IGraph(..)
  , interferenceGraph
  )
where

import qualified Graph                         as G
import qualified Flow

import           Control.Monad                  ( join )
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Trans.Writer     ( WriterT
                                                , runWriterT
                                                , tell
                                                )
import           Control.Monad.Trans.State      ( runStateT )
import           Data.Functor.Identity
import           Data.List
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as Vector

type TempId = Flow.TempId

newtype NodeId = NodeId Int

instance Show NodeId where
  show (NodeId i) = "NodeId_" ++ show i

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
                     , tnode :: Map TempId Node
                     , gtemp :: Map NodeId TempId
                     , moves :: [(Node, Node)] }
  deriving (Show)

-- | Compute the interference graph and the live (out) map.
interferenceGraph :: Flow.FlowGraph -> (IGraph, Map Flow.NodeId (Set TempId))
interferenceGraph flowGraph =
  let
    liveMap = buildLiveMap flowGraph
    (((_, moves'), tempIdToNode), graph') =
      runIdentity
        $ (runStateT . runWriterT . runWriterT) (buildGraph liveMap)
        $ G.newGraph
        $ NodeId 0
    tnodeMap = Map.fromList tempIdToNode
    gnodeMap =
      Map.fromList $ fmap (\(t, node) -> (G.nodeId node, t)) tempIdToNode
  in
    ( IGraph { graph = graph'
             , tnode = tnodeMap
             , gtemp = gnodeMap
             , moves = moves'
             }
    , liveMap
    )
 where
  buildGraph :: Map Flow.NodeId (Set TempId) -> IGraphBuilder ()
  buildGraph liveMap = do
    tempsAndNodes <- allocNodes nodeIds
    let tempToNode = Map.fromList tempsAndNodes
    (lift . tell) tempsAndNodes
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
    let accumTemps :: Map Flow.NodeId [TempId] -> Set TempId
        accumTemps tempMap =
            foldl' (\acc tempIds -> Set.union acc $ Set.fromList tempIds)
                   Set.empty
              $ Map.elems tempMap
        defs = accumTemps $ Flow.def flowGraph
        uses = accumTemps $ Flow.use flowGraph
    in  Set.union defs uses

  processNode
    :: Map TempId Node -> (Flow.NodeId, Set TempId) -> IGraphBuilder ()
  processNode tempToNode (flowNode, liveSet) = do
    let defs   = Flow.def flowGraph Map.! flowNode
    let isMove = Flow.ismove flowGraph Map.! flowNode
    mapM_ (addInterferenceEdges liveSet tempToNode isMove) defs
    case isMove of
      Just (dst, src) -> do
        let
          dstNode = case Map.lookup dst tempToNode of
            Just v -> v
            Nothing ->
              error $ "couldn't find " ++ show dst ++ " in " ++ show tempToNode
        let
          srcNode = case Map.lookup src tempToNode of
            Just v -> v
            Nothing ->
              error $ "couldn't find " ++ show src ++ " in " ++ show tempToNode
        tell [(dstNode, srcNode)]
        pure ()
      _ -> pure ()

  addInterferenceEdges
    :: Set TempId
    -> Map TempId Node
    -> Maybe (TempId, TempId)
    -> -- is the defined id a move?
       TempId
    -> IGraphBuilder ()
  addInterferenceEdges liveSet tempToNode isMove defdId =
    mapM_
        (\liveId ->
          let defdNode      = tempToNode Map.! defdId
              liveNode      = (tempToNode Map.! liveId)
              addEdgeAction = lift . lift $ G.addEdge defdNode liveNode
          in  case isMove of
                Just (_, src) ->
                  if liveId /= src then addEdgeAction else pure ()
                Nothing -> addEdgeAction
        )
      $ Set.toList liveSet

type Moves = [(Node, Node)]
type IGraphBuilder = WriterT Moves (WriterT [(TempId, Node)] GraphBuilder)

-- | computes live-out sets per FlowGraph node
buildLiveMap :: Flow.FlowGraph -> Map Flow.NodeId (Set TempId)
buildLiveMap g =
  let cfg                  = Flow.control g
      --flowKeys = Map.keys $ G.nodes cfg
      liveIn = Map.fromList $ fmap (\nodeId -> (nodeId, emptyBitset)) flowKeys
      liveOut              = liveIn
      quasiToposortedNodes = G.quasiTopoSort cfg
      flowKeys             = fmap (G.nodeId) quasiToposortedNodes
  in  Map.map bitsetToMap $ buildImpl flowKeys liveIn liveOut
 where
  emptyBitset :: Bitset
  emptyBitset = Vector.replicate (maxTempId + 1) False

  maxTempId :: Int
  maxTempId =
    let allDefs = join $ Map.elems $ Flow.def g
        allUse  = join $ Map.elems $ Flow.use g
    in  maximum $ allDefs ++ allUse

  bitsetToMap :: Bitset -> Set TempId
  bitsetToMap bs =
    let lst              = Vector.toList bs
        lstWithIx        = zip [0 ..] lst
        filtered         = filter snd lstWithIx
        appearingIndices = fmap fst filtered
    in  Set.fromList appearingIndices

  buildImpl
    :: [Flow.NodeId]
    -> Map Flow.NodeId Bitset
    -> Map Flow.NodeId Bitset
    -> Map Flow.NodeId Bitset
  buildImpl flowKeys liveIn liveOut =
    let liveIn'    = Map.fromList $ fmap (updateIn liveOut) flowKeys
        liveOut'   = Map.fromList $ fmap (updateOut liveIn) flowKeys
        shouldStop = liveIn' == liveIn && liveOut' == liveOut
    in  if shouldStop then liveOut else buildImpl flowKeys liveIn' liveOut'

  updateIn :: Map Flow.NodeId Bitset -> Flow.NodeId -> (Flow.NodeId, Bitset)
  updateIn liveOut nodeId =
    -- in[n] = use[n] \union (out[n]-def[n])
    let usesList = Flow.use g Map.! nodeId
        usesSet  = Set.fromList usesList
        out      = liveOut Map.! nodeId
        defList  = Flow.def g Map.! nodeId
        defSet   = Set.fromList defList
    in  (nodeId, Set.union usesSet $ Set.difference out defSet)

  updateOut :: Map Flow.NodeId Bitset -> Flow.NodeId -> (Flow.NodeId, Bitset)
  updateOut liveIn nodeId =
    -- out[n] = \Union_{s \in succ[n]} in[s]
    let succs     = G.succ $ G.nodes (Flow.control g) Map.! nodeId
        inOfSuccs = fmap (liveIn Map.!) succs
        out'      = foldl' Set.union Set.empty inOfSuccs
    in  (nodeId, out')

type Bitset = Vector Bool

_difference :: Bitset -> Bitset -> Bitset
_difference = Vector.zipWith diffFun
  where diffFun x y = if x && y then False else x

_union :: Bitset -> Bitset -> Bitset
_union = Vector.zipWith (||)
