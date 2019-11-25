module Flow where

import qualified Assem as A
import qualified Graph as G

import Control.Monad.Trans.State (runState)
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map


type TempId = Int

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

data FlowGraph = FlowGraph { control :: Graph
                           , def :: Map NodeId [TempId]
                           , use :: Map NodeId [TempId]
                           , ismove :: Map NodeId Bool }

instrsToGraph :: [A.Inst] -> (FlowGraph, [Node])
instrsToGraph insts =
  let
    ((nodes, defs, uses, isMoves), cfg) = runState buildGraph $ G.newGraph $ NodeId 0
  in
    ( FlowGraph { control=cfg
                , def=defs
                , use=uses
                , ismove=isMoves }
    , fmap snd nodes )
  where
    buildGraph :: GraphBuilder ( [(A.Inst, Node)]
                                 , Map NodeId [TempId]
                                 , Map NodeId [TempId]
                                 , Map NodeId Bool )
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

    allocNodes :: GraphBuilder [(A.Inst, Node)]
    allocNodes = mapM
                   (\inst -> do
                               node <- G.allocNode
                               pure (inst, node))
                   insts

    insertFallthroughs :: [(A.Inst, Node)] -> GraphBuilder ()
    insertFallthroughs nodes =
      mapM_
        (\((i1, n1), (_, n2)) ->
           case i1 of
             A.OPER { A.jump=Just _ } -> do pure ()
             _                        -> G.addEdge n1 n2
         )
        $ zip nodes $ tail nodes

    insertExplicitJumps :: [(A.Inst, Node)] -> GraphBuilder ()
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

    buildCFG :: GraphBuilder [(A.Inst, Node)]
    buildCFG = do
      nodes <- allocNodes
      _ <- insertFallthroughs nodes
      insertExplicitJumps nodes
      pure nodes
