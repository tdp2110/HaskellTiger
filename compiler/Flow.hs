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
    ((nodes, defs, uses, isMoves), cfg) = runState buildGraph G.newGraph
  in
    ( FlowGraph { control=cfg
                , def=defs
                , use=uses
                , ismove=isMoves }
    , fmap snd nodes )
  where
    buildGraph :: G.GraphBuilder ( [(A.Inst, G.Node)]
                                 , Map G.NodeId [TempId]
                                 , Map G.NodeId [TempId]
                                 , Map G.NodeId Bool )
    buildGraph = do
      nodes <- buildCFG
      let
        defs = Map.fromList $
                 fmap
                 (\(inst, node) -> ( G.nodeId node
                                   , case inst of
                                       A.OPER { A.operDst=dsts } -> dsts
                                       A.LABEL {} -> []
                                       A.MOVE { A.moveDst=dst } -> [dst] ))
                 nodes
        uses = Map.fromList $
                 fmap
                 (\(inst, node) -> ( G.nodeId node
                                   , case inst of
                                       A.OPER { A.operSrc=srcs } -> srcs
                                       A.LABEL {} -> []
                                       A.MOVE { A.moveSrc=src } -> [src] ))
                 nodes
        isMoves = Map.fromList $
                    fmap
                    (\(inst, node) -> ( G.nodeId node
                                      , case inst of
                                          A.MOVE {} -> True
                                          _         -> False ))
                    nodes
        in
        pure (nodes, defs, uses, isMoves)

    allocNodes :: G.GraphBuilder [(A.Inst, G.Node)]
    allocNodes = mapM
                   (\inst -> do
                               node <- G.allocNode
                               pure (inst, node))
                   insts

    insertFallthroughs :: [(A.Inst, G.Node)] -> G.GraphBuilder ()
    insertFallthroughs nodes =
      mapM_
        (\((i1, n1), (_, n2)) ->
           case i1 of
             A.OPER { A.jump=Just _ } -> do pure ()
             _                        -> G.addEdge n1 n2
         )
        $ zip nodes $ tail nodes

    insertExplicitJumps :: [(A.Inst, G.Node)] -> G.GraphBuilder ()
    insertExplicitJumps nodes =
      mapM_
        (\(i, n) ->
           case i of
             A.OPER { A.jump = Just jumpTargetLabs } ->
               let
                 jumpTargetNodes = fmap
                                     (\lab -> find
                                                (\(inst, _) -> case inst of
                                                     A.LABEL { A.lab=lab' } -> lab == lab'
                                                     _                      -> False)
                                                nodes)
                                     jumpTargetLabs
                 jumpTargetNodes' = case sequence jumpTargetNodes of
                                      Just targets -> fmap snd targets
                                      _            -> error "invalid instruction list"
               in do
                 mapM_
                   (G.addEdge n)
                   jumpTargetNodes'
             _ -> do pure ()
         )
         nodes

    buildCFG :: G.GraphBuilder [(A.Inst, G.Node)]
    buildCFG = do
      nodes <- allocNodes
      _ <- insertFallthroughs nodes
      insertExplicitJumps nodes
      pure nodes
