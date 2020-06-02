module AssemOptim
  ( Pass
  , pruneDefdButNotUsed
  , removeTrivialJumps
  )
where

import qualified Assem                         as A
import qualified Flow                          as F
import qualified Graph                         as G

import           Data.Foldable                  ( foldl' )
import qualified Data.Map                      as Map


type Pass
  = ([A.Inst], (F.FlowGraph, [F.Node])) -> ([A.Inst], (F.FlowGraph, [F.Node]))

pruneDefdButNotUsed :: Pass
pruneDefdButNotUsed (insts, (F.FlowGraph { F.control = control, F.use = use }, _))
  = let getDefaultZero = Map.findWithDefault (0 :: Int)
        useCounts =
            foldl'
                (\acc nodeId ->
                  let uses = use Map.! nodeId
                      acc2 = foldl'
                        (\acc3 usedId ->
                          let ct   = getDefaultZero usedId acc3
                              acc4 = Map.insert usedId (ct + 1) acc3
                          in  acc4
                        )
                        acc
                        uses
                  in  acc2
                )
                Map.empty
              $ Map.keys
              $ G.nodes control
        insts' = filter
          (\inst -> case inst of
            A.MOVE { A.moveDst = defdId } ->
              getDefaultZero defdId useCounts /= 0
            _ -> True
          )
          insts
    in  (insts', F.instrsToGraph insts')

removeTrivialJumps :: Pass
removeTrivialJumps (insts, (_, flowNodes)) =
  let
    nodes = zip insts $ fmap G.nodeId flowNodes
    potentialTrivialJumps =
      extractPotentialTrivialJumps =<< zip nodes (tail nodes)
    labelJumpCounts = foldl' labelJumpCountAccumulator Map.empty insts
    trivialJumps    = filter (\(_, _, lab) -> hasCountOne lab labelJumpCounts)
                             potentialTrivialJumps
    nodesToDelete = (\(n1, n2, _) -> [n1, n2]) =<< trivialJumps
    prunedNodes   = filter (\(_, n) -> n `notElem` nodesToDelete) nodes
    insts'        = fmap fst prunedNodes
  in
    (insts', F.instrsToGraph insts')
 where
  hasCountOne :: A.Label -> Map.Map A.Label Int -> Bool
  hasCountOne lab countMap = case Map.lookup lab countMap of
    Just ct -> ct == 1
    _       -> False

  labelJumpCountAccumulator
    :: Map.Map A.Label Int -> A.Inst -> Map.Map A.Label Int
  labelJumpCountAccumulator acc A.OPER { A.jump = Just jumpTargets } = foldl'
    (\acc' jt ->
      let ct = Map.findWithDefault 0 jt acc' in Map.insert jt (ct + 1) acc'
    )
    acc
    jumpTargets
  labelJumpCountAccumulator acc _ = acc

  extractPotentialTrivialJumps ((A.OPER { A.jump = Just [lab1] }, n1), (A.LABEL { A.lab = lab2 }, n2))
    = [ (n2, n1, lab1) | lab1 == lab2 ]
  extractPotentialTrivialJumps _ = []
