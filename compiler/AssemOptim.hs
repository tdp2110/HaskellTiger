module AssemOptim where

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
