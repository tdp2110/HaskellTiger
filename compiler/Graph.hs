module Graph
  ( NodeId(..)
  , Node(..)
  , GraphBuilder
  , Graph(..)
  , addEdge
  , newGraph
  , allocNode
  , edges
  , hasEdge
  , toDot
  , exitNodes
  , quasiTopoSort
  , newNode
  , mkEdge
  )
where

import           Control.Monad                  ( when )
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Trans.Writer     ( WriterT
                                                , execWriter
                                                , runWriterT
                                                , tell
                                                )
import           Control.Monad.Trans.State      ( StateT
                                                , State
                                                , get
                                                , put
                                                , runState
                                                )
import           Data.Functor.Identity
import           Data.DList                     ( DList
                                                , toList
                                                , singleton
                                                )
import           Data.List
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set

import           Prelude                 hiding ( succ
                                                , pred
                                                )


class (Ord a, Show a) => NodeId a where
  incrId :: a -> a

data Node a = Node { succ :: [a]
                   , pred :: [a]
                   , nodeId :: a }
  deriving (Show)

freshNode :: a -> Node a
freshNode nId = Node { succ = [], pred = [], nodeId = nId }

data Graph a = Graph { nodes :: Map a (Node a)
                     , nextId :: a }
  deriving (Show)

exitNodes :: NodeId a => Graph a -> [Node a]
exitNodes g = filter isExitNode $ Map.elems $ nodes g
  where isExitNode = null . succ

type TopoSorter a = WriterT (DList a) (StateT (Set a) Identity)

quasiTopoSort :: NodeId a => Graph a -> [Node a]
quasiTopoSort g =
  let unvisitedIds = Set.fromList $ Map.keys $ nodes g
      (((), dlist1), unvisitedIds') =
          (flip runState unvisitedIds . runWriterT) $ quasiTopoSortM g
      (((), dlist2), _) =
          (flip runState unvisitedIds' . runWriterT) $ quasiTopoSortM g

      ids1   = toList dlist1
      ids2   = toList dlist2
      allIds = reverse $ ids1 ++ ids2
  in  fmap (nodes g Map.!) allIds

quasiTopoSortM :: NodeId a => Graph a -> TopoSorter a ()
quasiTopoSortM g = let exits = exitNodes g in mapM_ (dfs g) exits

dfs :: NodeId a => Graph a -> Node a -> TopoSorter a ()
dfs g node =
  let nodeId_ = nodeId node
  in  do
        unvisited <- lift get
        when (Set.member nodeId_ unvisited)
          $ let unvisited' = Set.delete nodeId_ unvisited
            in  do
                  (lift . put) unvisited'
                  mapM_ (dfs g) $ fmap (nodes g Map.!) $ pred node
                  tell $ singleton nodeId_
                  pure ()


-- | produce a repr of a graph in the "dot" language
toDot :: Show a => Graph a -> String
toDot g =
  let graphBody = execWriter $ dotBuilder $ nodes g
  in  "graph {\n" ++ graphBody ++ "}"
 where
  dotBuilder m = mapM_ processNode $ Map.toList m

  processNode (nId, Node { succ = succs }) = do
    tell $ fmtNode nId
    fmtSuccs nId succs

  dotId n = "node_" ++ show n
  indent = "    "
  fmtNode n = indent ++ dotId n ++ " [label=\"" ++ show n ++ "\"];\n"
  fmtSuccs n = mapM_ (fmtEdge n)
  fmtEdge n1 n2 = tell $ indent ++ dotId n1 ++ " -- " ++ dotId n2 ++ ";\n"

newGraph :: a -> Graph a
newGraph firstId = Graph { nodes = Map.empty, nextId = firstId }

newNode :: NodeId a => Graph a -> (Node a, Graph a)
newNode g@Graph { nextId = nId } =
  let node = freshNode nId
  in  (node, g { nodes = Map.insert nId node $ nodes g, nextId = incrId nId })

mkEdge :: NodeId a => Graph a -> a -> a -> Graph a
mkEdge g id1 id2 =
  let nodes_g   = nodes g
      n1        = nodes_g Map.! id1
      n2        = nodes_g Map.! id2
      n1'       = n1 { succ = id2 : succ n1 }
      n2'       = n2 { pred = id1 : pred n2 }
      nodes_g'  = Map.insert id1 n1' nodes_g
      nodes_g'' = Map.insert id2 n2' nodes_g'
  in  if hasEdge g id1 id2 then g else g { nodes = nodes_g'' }

hasEdge :: NodeId a => Graph a -> a -> a -> Bool
hasEdge g id1 id2 = let n1 = nodes g Map.! id1 in elem id2 $ succ n1

edges :: NodeId a => Graph a -> [(a, a)]
edges g = execWriter accumEdges
 where
  accumEdges =
    mapM_ (\(nId, n) -> mapM_ (\sId -> tell [(nId, sId)]) $ succ n)
      $ Map.toList
      $ nodes g


type GraphBuilder a = State (Graph a)

allocNode :: NodeId a => GraphBuilder a (Node a)
allocNode = do
  g <- get
  let (node, g') = newNode g
  put g'
  pure node

addEdge :: NodeId a => Node a -> Node a -> GraphBuilder a ()
addEdge n1 n2 = do
  g <- get
  let g' = mkEdge g (nodeId n1) (nodeId n2)
  put g'
  pure ()
