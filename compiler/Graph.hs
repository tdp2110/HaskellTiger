module Graph where

import Control.Monad.Trans.State (State, get, put, runState)
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

import Prelude hiding (succ, pred)


type NodeId = Int

data Node = Node { succ :: [NodeId]
                 , pred :: [NodeId]
                 , nodeId :: NodeId }

freshNode :: NodeId -> Node
freshNode nId = Node { succ=[]
                     , pred=[]
                     , nodeId=nId }

data Graph = Graph { nodes :: Map Int Node
                   , nextId :: NodeId }

newGraph :: Graph
newGraph = Graph { nodes=Map.empty
                 , nextId=0 }

newNode :: Graph -> (Node, Graph)
newNode g@(Graph{nextId=nId}) =
  let
    node = freshNode nId
  in
    ( node
    , g{ nodes=Map.insert nId node $ nodes g
       , nextId=nId + 1 } )

mkEdge :: Graph -> NodeId -> NodeId -> Graph
mkEdge g id1 id2 =
  let
    nodes_g = nodes g
    n1 = nodes_g Map.! id1
    n2 = nodes_g Map.! id2
    n1' = n1{succ=[id2] ++ succ n1}
    n2' = n2{pred=[id2] ++ pred n2}
    nodes_g' = Map.insert id1 n1' nodes_g
    nodes_g'' = Map.insert id2 n2' nodes_g'
  in
    g{nodes=nodes_g''}

rmEdge :: Graph -> NodeId -> NodeId -> Graph
rmEdge g id1 id2 =
  let
    nodes_g = nodes g
    n1 = nodes_g Map.! id1
    n2 = nodes_g Map.! id2
    n1' = n1{succ=delete id2 $ succ n1}
    n2' = n2{pred=delete id1 $ pred n2}
    nodes_g' = Map.insert id1 n1' nodes_g
    nodes_g'' = Map.insert id2 n2' nodes_g'
  in
    g{nodes=nodes_g''}

type GraphBuilder = State Graph

allocNode :: GraphBuilder Node
allocNode = do
  g <- get
  let
    (node, g') = newNode g
    in do
    put g'
    pure node

addEdge :: Node -> Node -> GraphBuilder ()
addEdge n1 n2 = do
  g <- get
  let
    g' = mkEdge g (nodeId n1) (nodeId n2)
    in do
    put g'
    pure ()

delEdge :: Node -> Node -> GraphBuilder ()
delEdge n1 n2 = do
  g <- get
  let
    g' = rmEdge g (nodeId n1) (nodeId n2)
    in do
    put g'
    pure ()
