module AssemOptim
  ( optimizePreRegAlloc
  , optimizePostRegAlloc
  )
where

import qualified Assem                         as A
import qualified Flow                          as F
import qualified Graph                         as G
import qualified Temp

import qualified Data.Bifunctor
import           Data.Foldable                  ( foldl' )
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import qualified Data.Text                     as Text
import           Data.List                      ( zip4 )

type CFG = ([A.Inst], (F.FlowGraph, [F.Node]))
type PassKernel = CFG -> [A.Inst]
type Pass = CFG -> CFG

makePass :: PassKernel -> Pass
makePass kernel input =
  let instrs = kernel input in (instrs, F.instrsToGraph instrs)

optimizePreRegAlloc :: [A.Inst] -> CFG
optimizePreRegAlloc =
  optimize $ fmap makePass [pruneDefdButNotUsed, removeTrivialJumps]

optimizePostRegAlloc :: [A.Inst] -> CFG
optimizePostRegAlloc =
  optimize $ fmap makePass [chaseJumps, removeTrivialJumps]

optimize :: [Pass] -> [A.Inst] -> CFG
optimize passes insts =
  let (flowGraph, nodes) = F.instrsToGraph insts
      input              = (insts, (flowGraph, nodes))
  in  foldl' applyPass input passes
 where
  applyPass :: CFG -> Pass -> CFG
  applyPass input pass = pass input

pruneDefdButNotUsed :: PassKernel
pruneDefdButNotUsed (insts, (F.FlowGraph { F.control = control, F.use = use }, _))
  = filter shouldKeep insts
 where
  shouldKeep :: A.Inst -> Bool
  shouldKeep A.MOVE { A.moveDst = defdId }      = idHasUse defdId
  shouldKeep A.STORECONST { A.strDst = defdId } = idHasUse defdId
  shouldKeep A.OPER { A.operDst = defdIds, A.hasSideEffect = False } =
    any idHasUse defdIds
  shouldKeep _ = True

  getDefaultZero = Map.findWithDefault (0 :: Int)
  idHasUse defdId = getDefaultZero defdId useCounts /= 0
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

chaseJumps :: PassKernel
chaseJumps (insts, (F.FlowGraph { F.control = cfg }, flowNodes)) =
  let nodeIds = fmap G.nodeId flowNodes
      label2NodeId =
          Map.fromList
            $ fmap (Data.Bifunctor.first A.lab)
            $ filter (\(i, _) -> isLabel i)
            $ zip insts nodeIds
      nodeId2Inst = Map.fromList $ zip nodeIds insts
      insts'      = fmap (chase label2NodeId nodeId2Inst) insts
  in  insts'
 where
  isLabel A.LABEL{} = True
  isLabel _         = False

  chase label2NodeId nodeId2Inst inst@A.OPER { A.jump = Just [jumpTargetLab], A.operSrc = [], A.operDst = [] }
    = let jumpTargetNodeId = label2NodeId Map.! jumpTargetLab
          jumpTargetNode   = G.nodes cfg Map.! jumpTargetNodeId
          succs            = G.succ jumpTargetNode
      in  case succs of
            [succId] -> case nodeId2Inst Map.! succId of
              inst'@A.OPER { A.jump = Just [_], A.operSrc = [], A.operDst = [] }
                -> inst'
              _ -> inst
            _ -> inst
  chase _ _ inst = inst

eliminateDeadCode :: PassKernel
eliminateDeadCode (insts, (F.FlowGraph { F.control = _ }, nodes)) =
  let nodesWithNoPred = fmap nodeHasNoPred nodes
      maybeMains      = fmap instMaybeIsMain insts
      shouldDeletes =
          (\(hasNoPred, isLabel, isNotCalled, maybeMain) ->
              hasNoPred && isLabel && isNotCalled && not maybeMain
            )
            <$> zip4 nodesWithNoPred labelNodes uncalledNodes maybeMains
      labsToDelete =
          (\(inst, shouldDelete) -> case inst of
              A.LABEL { A.lab = lab } -> [ lab | shouldDelete ]
              _                       -> []
            )
            =<< zip insts shouldDeletes
  in  error $ show labsToDelete
 where
  nodeHasNoPred node = null $ G.pred node

  instMaybeIsMain A.LABEL { A.lab = lab } =
    let labName = Text.unpack $ Temp.name lab
    in  labName == "main" || labName == "_main"
  instMaybeIsMain _ = False

  labelNodes = fmap
    (\inst -> case inst of
      A.LABEL{} -> True
      _         -> False
    )
    insts

  uncalledNodes = fmap
    (\inst -> case inst of
      A.LABEL { A.lab = lab } -> not $ Set.member lab callTargetLabs
      _                       -> False
    )
    insts

  callTargetLabs = foldl'
    (\acc inst -> case inst of
      A.OPER { A.calls = Just lab } -> Set.insert lab acc
      _                             -> acc
    )
    Set.empty
    insts

-- | remove
removeTrivialJumps :: PassKernel
removeTrivialJumps (insts, (_, flowNodes)) =
  let nodes           = zip insts $ fmap G.nodeId flowNodes
      trivialJumps    = extractTrivialJumps =<< zip nodes (tail nodes)
      labelJumpCounts = foldl' labelJumpCountAccumulator Map.empty insts
      nodesToDelete =
          (\(n1, n2, lab) ->
              if hasCountOne lab labelJumpCounts then [n1, n2] else [n2]
            )
            =<< trivialJumps
      prunedNodes = filter (\(_, n) -> n `notElem` nodesToDelete) nodes
  in  fmap fst prunedNodes
 where
  hasCountOne :: A.Label -> Map.Map A.Label Int -> Bool
  hasCountOne lab countMap = case Map.lookup lab countMap of
    Just ct -> ct == 1
    Nothing -> False

  labelJumpCountAccumulator
    :: Map.Map A.Label Int -> A.Inst -> Map.Map A.Label Int
  labelJumpCountAccumulator acc A.OPER { A.jump = Just jumpTargets } = foldl'
    (\acc' jt ->
      let ct = Map.findWithDefault 0 jt acc' in Map.insert jt (ct + 1) acc'
    )
    acc
    jumpTargets
  labelJumpCountAccumulator acc _ = acc

  extractTrivialJumps ((A.OPER { A.jump = Just [lab1], A.operSrc = [], A.operDst = [] }, n1), (A.LABEL { A.lab = lab2 }, n2))
    = [ (n2, n1, lab1) | lab1 == lab2 ]
  extractTrivialJumps _ = []
