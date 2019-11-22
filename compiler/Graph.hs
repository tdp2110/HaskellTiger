module Graph where

import Data.Map (Map)
import qualified Data.Map as Map

import Prelude hiding (succ, pred)


data InfList a = InfList a (InfList a)

genInfList :: (a -> a) -> a -> InfList a
genInfList f elt = InfList elt (genInfList f $ f elt)

type NodeId = Int

data Node = Node { succ :: [Node]
                 , pred :: [Node]
                 , nodeId :: NodeId }

freshNode :: NodeId -> Node
freshNode nId = Node { succ=[]
                     , pred=[]
                     , nodeId=nId }

data Graph = Graph { nodes :: Map Int Node
                   , _idSupply :: InfList Int }

newGraph :: Graph
newGraph = Graph { nodes=Map.empty
                 , _idSupply=genInfList (+1) 0 }

newNode :: Graph -> (Node, Graph)
newNode g@(Graph{_idSupply=(InfList nId nIds@(InfList _ _))}) =
  let
    node = freshNode nId
  in
    ( node
    , g{ nodes=Map.insert nId node $ nodes g
       , _idSupply=nIds } )

mkEdge :: Graph -> Int -> Int -> Graph
mkEdge = undefined

rmEdge :: Graph -> Int -> Int -> Graph
rmEdge = undefined
