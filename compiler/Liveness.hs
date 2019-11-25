module Liveness where

import qualified Assem as A
import qualified Graph as G
import qualified Flow as Flow

import Control.Monad.Trans.State (runState)
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

buildLiveMap :: Flow.Graph -> Map Flow.NodeId (Set TempId)
buildLiveMap = undefined
