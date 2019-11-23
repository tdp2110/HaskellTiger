module Flow where

import qualified Assem as A
import qualified Graph as G

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Trans.State (runState)


type TempId = Int

data FlowGraph = FlowGraph { control :: G.Graph
                           , def :: Map G.NodeId [TempId]
                           , use :: Map G.NodeId [TempId]
                           , ismove :: Map G.NodeId Bool }

instrsToGraph :: [A.Inst] -> (FlowGraph, [G.Node])
instrsToGraph insts =
  let
    ((), g) = runState (buildGraph insts) G.newGraph
  in
    undefined
  where
    buildGraph :: [A.Inst] -> G.GraphBuilder ()
    buildGraph = undefined

    allocNodes :: [A.Inst] -> G.GraphBuilder [(A.Inst, G.Node)]
    allocNodes insts = mapM
                         (\inst -> do
                                     node <- G.allocNode
                                     pure (inst, node))
                         insts

    buildCFG :: [A.Inst] -> G.GraphBuilder ()
    buildCFG insts = do
     nodes <- allocNodes insts
     mapM_
       (\((i1, n1), (i2, n2)) ->
          case i1 of
            A.OPER { A.jump=Just targets } -> undefined
            _ -> G.addEdge n1 n2
        )
       (zip nodes $ tail nodes)
