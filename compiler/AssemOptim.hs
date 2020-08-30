module AssemOptim
  ( optimizePreRegAlloc
  , optimizePostRegAlloc
  )
where

import qualified Assem                         as A
import qualified Flow                          as F
import qualified Graph                         as G

import qualified Data.Bifunctor
import           Data.Foldable                  ( foldl'
                                                , find
                                                )
import qualified Data.Map                      as Map
import qualified Data.Maybe
import qualified Data.Set                      as Set
import           Data.List                      ( zip3
                                                , zip4
                                                )
import qualified Data.Text                     as T

type CFG = ([A.Inst], (F.FlowGraph, [F.Node]))
type PassKernel = CFG -> [A.Inst]
type Pass = CFG -> CFG


makePass :: PassKernel -> Pass
makePass kernel input =
  let instrs = kernel input in (instrs, F.instrsToGraph instrs)

optimizePreRegAlloc :: [A.Inst] -> CFG
optimizePreRegAlloc = optimize $ fmap
  makePass
  [ pruneDefdButNotUsed
  , removeTrivialJumps
  , eliminateDeadCode
  , propagateConstants
  , pruneDefdButNotUsed
  ]

optimizePostRegAlloc :: [A.Inst] -> CFG
optimizePostRegAlloc = optimize $ fmap
  makePass
  [ chaseJumps
  , removeTrivialJumps
  , eliminateDeadCode
  , removeUnneededLabels
  , eliminateRedundantStores
  ]

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

propagateConstants :: PassKernel
propagateConstants (insts, (F.FlowGraph { F.control = cfg }, nodes)) =
  let nodeIds                    = G.nodeId <$> nodes
      topoOrderedNodes           = reverse $ G.quasiTopoSort cfg
      nodeIdToInst               = Map.fromList $ zip nodeIds insts
      (_, finalNodeReplacements) = foldl'
        (\(constMap, nodeReplacements) node -> if length (G.pred node) > 1
          then (Map.empty, nodeReplacements)
          else
            let nodeId = G.nodeId node
                inst   = nodeIdToInst Map.! nodeId
            in  case inst of
                  A.STORECONST { A.strDst = strDst, A.strVal = strVal } ->
                    (Map.insert strDst strVal constMap, nodeReplacements)
                  A.MOVE { A.moveDst = moveDst, A.moveSrc = moveSrc } ->
                    case Map.lookup moveSrc constMap of
                      Just c ->
                        ( Map.insert moveDst c constMap
                        , Map.insert nodeId
                                     (mkStoreConst moveDst c)
                                     nodeReplacements
                        )
                      Nothing ->
                        (Map.delete moveDst constMap, nodeReplacements)
                  _ -> (constMap, nodeReplacements)
        )
        (Map.empty, Map.empty)
        topoOrderedNodes
      insts' =
          (   (\(inst, nodeId) ->
                Data.Maybe.fromMaybe inst (Map.lookup nodeId finalNodeReplacements)
              )
          <$> zip insts nodeIds
          )
  in  insts'
 where
  mkStoreConst moveDst c = A.STORECONST
    { A.assem  = T.pack $ "\tmov `d0, " ++ show c
    , A.strDst = moveDst
    , A.strVal = c
    }

eliminateRedundantStores :: PassKernel
eliminateRedundantStores (insts, (F.FlowGraph { F.control = cfg }, nodes)) =
  let shouldKeeps = shouldKeep <$> zip insts nodes
      insts'      = fmap fst $ filter snd $ zip insts shouldKeeps
  in  insts'
 where
  shouldKeep (inst, node) =
    let nodeIdToInst = Map.fromList $ zip (fmap G.nodeId nodes) insts
        segment :: [A.Inst]
        segment = computeSegment nodeIdToInst (inst, node)
    in  case inst of
          A.OPER{}                           -> True
          A.LABEL{}                          -> True
          A.MOVE { A.moveDst = moveDst }     -> isMaybeUsed moveDst segment
          A.STORECONST { A.strDst = strDst } -> isMaybeUsed strDst segment

  computeSegment :: Map.Map F.NodeId A.Inst -> (A.Inst, F.Node) -> [A.Inst]
  computeSegment nodeIdToInst (inst, node) =
    let (_ : res) = computeSegmentImpl nodeIdToInst
                                       (inst, node)
                                       []
                                       (Set.singleton $ G.nodeId node)
    in  res

  computeSegmentImpl
    :: Map.Map F.NodeId A.Inst
    -> (A.Inst, F.Node)
    -> [A.Inst]
    -> Set.Set F.NodeId
    -> [A.Inst]
  computeSegmentImpl nodeIdToInst (inst, node) initialSegment visitedIds =
    let successors = G.succ node
    in  case successors of
          [uniqueSucessorId] -> if uniqueSucessorId `Set.member` visitedIds
            then initialSegment
            else
              let successorNode = G.nodes cfg Map.! uniqueSucessorId
                  successorInst = nodeIdToInst Map.! uniqueSucessorId
              in  computeSegmentImpl nodeIdToInst
                                     (successorInst, successorNode)
                                     (initialSegment ++ [inst])
                                     (Set.insert uniqueSucessorId visitedIds)
          _ -> initialSegment

  isMaybeUsed :: Int -> [A.Inst] -> Bool
  isMaybeUsed reg segment =
    let uses          = fmap (instUses reg) segment
        defs          = fmap (instDefs reg) segment
        maybeFirstUse = fmap fst $ find snd $ zip [0 :: Int ..] uses
        maybeFirstDef = fmap fst $ find snd $ zip [0 :: Int ..] defs
    in  case (maybeFirstDef, maybeFirstUse) of
          (Just firstDef, Just firstUse) -> firstUse <= firstDef
          (Just _       , Nothing      ) -> False
          _                              -> True

  instUses :: Int -> A.Inst -> Bool
  instUses reg inst = case inst of
    A.OPER { A.operSrc = operSrc } -> reg `elem` operSrc
    A.LABEL{}                      -> False
    A.MOVE { A.moveSrc = moveSrc } -> reg == moveSrc
    A.STORECONST{}                 -> False

  instDefs :: Int -> A.Inst -> Bool
  instDefs reg inst = case inst of
    A.OPER { A.operDst = operDst }     -> reg `elem` operDst
    A.LABEL{}                          -> False
    A.MOVE { A.moveDst = moveDst }     -> reg == moveDst
    A.STORECONST { A.strDst = strDst } -> reg == strDst

eliminateDeadCode :: PassKernel
eliminateDeadCode (insts, (F.FlowGraph { F.control = cfg }, nodes)) =
  case insts of
    (entryInst : _) ->
      let nodesWithNoPred = nodeHasNoPred <$> nodes
          nodeIds         = G.nodeId <$> nodes
          entryId         = head nodeIds
          shouldDeletes =
              (\(hasNoPred, isLabel, isNotCalled, inst) ->
                  hasNoPred && isLabel && isNotCalled && inst /= entryInst
                )
                <$> zip4 nodesWithNoPred labelNodes uncalledNodes insts
          labelNodesToDelete =
              Set.fromList $ fst <$> filter snd (zip nodeIds shouldDeletes)
          topoOrderedNodes = reverse $ G.quasiTopoSort cfg
          nodesToDelete    = foldl'
            (\acc node ->
              let nodeId       = G.nodeId node
                  predecessors = G.pred node
                  acc' =
                      if nodeId
                           /= entryId
                           && (not . null) predecessors
                           && all (`Set.member` acc) predecessors
                        then Set.insert nodeId acc
                        else acc
              in  acc'
            )
            labelNodesToDelete
            topoOrderedNodes
          insts' =
              fmap fst
                $ filter (\(_, nodeId) -> not $ Set.member nodeId nodesToDelete)
                $ zip insts nodeIds
      in  insts'
    _ -> error $ "malformed insts list: " ++ show insts
 where
  nodeHasNoPred node = null $ G.pred node

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

removeUnneededLabels :: PassKernel
removeUnneededLabels (insts, _) = case insts of
  (entry@A.LABEL{} : _) ->
    let shouldRemoveInst =
            (   (\(isUncalled, isNotJumpedTo, inst) ->
                  isUncalled && isNotJumpedTo && inst /= entry
                )
            <$> zip3 uncalledNodes unjumpedToNodes insts
            )
        insts' = fmap fst $ filter (not . snd) $ zip insts shouldRemoveInst
    in  insts'
  _ -> error $ "malformed insts list: " ++ show insts
 where
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

  unjumpedToNodes = fmap
    (\inst -> case inst of
      A.LABEL { A.lab = lab } -> not $ Set.member lab jumpTargetLabs
      _                       -> False
    )
    insts

  jumpTargetLabs = foldl'
    (\acc inst -> case inst of
      A.OPER { A.jump = Just labs } -> foldl' (flip Set.insert) acc labs
      _                             -> acc
    )
    Set.empty
    insts
