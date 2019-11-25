module Liveness where

import qualified Assem as A
import qualified Graph as G
import qualified Flow as Flow

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

data IGraph = IGraph { graph :: Graph
                     , tnode :: TempId -> Node
                     , gtemp :: Node -> TempId
                     , moves :: [(Node, Node)] }

interferenceGraph :: Flow.Graph -> (IGraph, Flow.Node -> [TempId])
interferenceGraph = undefined

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
