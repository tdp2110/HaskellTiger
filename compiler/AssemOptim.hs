module AssemOptim where

import qualified Assem as A
import qualified Flow as F
import qualified Graph as G

import Data.Foldable (foldl')
import qualified Data.Map as Map


pruneDefdButNotUsed :: [A.Inst] -> (F.FlowGraph, [F.Node]) -> ([A.Inst], (F.FlowGraph, [F.Node]))
pruneDefdButNotUsed insts ( F.FlowGraph { F.control=control
                                        , F.use=use }
                          , _) =
  let
    useCounts = foldl'
                  (\acc nodeId ->
                    let
                      uses = use Map.! nodeId
                      acc2 = foldl'
                               (\acc3 usedId ->
                                 let
                                   ct = Map.findWithDefault (0 :: Int) usedId acc3
                                   acc4 = Map.insert usedId (ct + 1) acc3
                                 in
                                   acc4)
                               acc
                               uses
                    in
                      acc2)
                  Map.empty
                  $ Map.keys $ G.nodes control
    insts' = filter
                 (\inst ->
                   case inst of
                     A.MOVE { A.moveDst=defdId } ->
                       useCounts Map.! defdId /= 0
                     _ -> True)
                 insts
  in
    (insts', F.instrsToGraph insts')
