module Flow where

import qualified Assem as A
import qualified Graph as G

import Data.List
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
    ((), g) = runState buildGraph G.newGraph
  in
    undefined
  where
    buildGraph :: G.GraphBuilder ()
    buildGraph = undefined

    allocNodes :: G.GraphBuilder [(A.Inst, G.Node)]
    allocNodes = mapM
                   (\inst -> do
                               node <- G.allocNode
                               pure (inst, node))
                   insts

    buildCFG :: G.GraphBuilder ()
    buildCFG = do
     nodes <- allocNodes
     mapM_ -- compute fallthroughs
       (\((i1, n1), (_, n2)) ->
          case i1 of
            A.OPER { A.jump=Just _ } -> do pure ()
            _ -> G.addEdge n1 n2
        )
       $ zip nodes $ tail nodes
     mapM_ -- compute explicit jumps
       (\(i, n) ->
          case i of
            A.OPER { A.jump = Just jumpTargetLabs } ->
              let
                jumpTargetNodes = fmap
                                    (\lab -> find
                                               (\(inst, _) -> case inst of
                                                    A.LABEL { A.lab=lab' } -> lab == lab'
                                                    _ -> False)
                                               nodes)
                                    jumpTargetLabs
                jumpTargetNodes' = case sequence jumpTargetNodes of
                                     Just targets -> fmap snd targets
                                     _ -> error "invalid instruction list"
              in do
                mapM_
                  (G.addEdge n)
                  jumpTargetNodes'
            _ -> do pure ()
        )
        nodes
