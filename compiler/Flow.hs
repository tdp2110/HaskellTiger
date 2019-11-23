module Flow where

import qualified Assem as A
import qualified Graph as G

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Trans.State (State, get, put, runState)


type TempId = Int

data FlowGraph = FlowGraph { control :: G.Graph
                           , def :: Map G.NodeId [TempId]
                           , use :: Map G.NodeId [TempId]
                           , ismove :: Map G.NodeId Bool }

type GraphBuilder = State G.Graph

instrsToGraph :: [A.Inst] -> (FlowGraph, [G.Node])
instrsToGraph insts =
  let
    ((), g) = runState (buildGraph insts) G.newGraph
  in
    undefined
  where
    buildGraph :: [A.Inst] -> GraphBuilder ()
    buildGraph = undefined

    allocNode :: GraphBuilder G.Node
    allocNode = do
      g <- get
      let
        (node, g') = G.newNode g
        in do
        put g'
        pure node

    addEdge :: G.Node -> G.Node -> GraphBuilder ()
    addEdge n1 n2 = do
      g <- get
      let
        g' = G.mkEdge g (G.nodeId n1) (G.nodeId n2)
        in do
        put g'
        pure ()

    allocNodes :: [A.Inst] -> GraphBuilder [(A.Inst, G.Node)]
    allocNodes insts = mapM
                         (\inst -> do
                                     node <- allocNode
                                     pure (inst, node))
                         insts

    buildCFG :: [A.Inst] -> GraphBuilder ()
    buildCFG insts = do
     nodes <- allocNodes insts
     mapM_
       (\((i1, n1), (i2, n2)) ->
          case i1 of
            A.OPER { A.jump=Just targets } -> undefined
            _ -> addEdge n1 n2
        )
       (zip nodes $ tail nodes)
