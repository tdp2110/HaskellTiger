module IROptim
  ( optimizeBasicBlock
  )
where

import qualified Canon
import qualified TreeIR                        as T

import qualified Data.Map                      as Map
import           Control.Monad.Trans.State      ( State
                                                , get
                                                , put
                                                , evalState
                                                )
import           Data.Bits                      ( shiftL
                                                , shiftR
                                                , xor
                                                )


optimizeBasicBlock :: Canon.Block -> Canon.Block
optimizeBasicBlock bb =
  let bb' = (foldConstants . propagateConstants) bb
  in  if bb' == bb then bb else optimizeBasicBlock bb'

foldConstants :: Canon.Block -> Canon.Block
foldConstants bb = evalState (mapM foldConstantsM bb) Map.empty
 where
  foldConstantsM :: T.Stm -> State (Map.Map Int Int) T.Stm
  foldConstantsM stm@(T.MOVE (T.TEMP dst, T.TEMP src)) = do
    constMap <- get
    case Map.lookup src constMap of
      Just constVal ->
        let constMap' = Map.insert dst constVal constMap
        in  do
              put constMap'
              pure $ T.MOVE (T.TEMP dst, T.CONST constVal)
      _ -> do
        deleteFromConstMap stm
        pure stm
  foldConstantsM stm@(T.MOVE (T.TEMP dst, T.CONST constVal)) = do
    constMap <- get
    let constMap' = Map.insert dst constVal constMap
    put constMap'
    pure stm
  foldConstantsM stm@(T.MOVE (tmp@(T.TEMP _), e)) = do
    deleteFromConstMap stm
    constMap <- get
    pure $ T.MOVE (tmp, constFoldExp constMap e)
  foldConstantsM (T.MOVE (e1, e2)) = do
    constMap <- get
    pure $ T.MOVE (e1, constFoldExp constMap e2)
  foldConstantsM (T.EXP e) = do
    constMap <- get
    pure $ T.EXP $ constFoldExp constMap e
  foldConstantsM (T.JUMP (e, labs)) = do
    constMap <- get
    pure $ T.JUMP (constFoldExp constMap e, labs)
  foldConstantsM (T.CJUMP (op, e1, e2, lab1, lab2)) = do
    constMap <- get
    pure $ T.CJUMP
      (op, constFoldExp constMap e1, constFoldExp constMap e2, lab1, lab2)
  foldConstantsM (T.SEQ (stm1, stm2)) = do
    stm1' <- foldConstantsM stm1
    stm2' <- foldConstantsM stm2
    pure $ T.SEQ (stm1', stm2')
  foldConstantsM lab@(T.LABEL _) = pure lab

  constFoldExp :: Map.Map Int Int -> T.Exp -> T.Exp
  constFoldExp _ c@(  T.CONST _) = c
  constFoldExp _ nm@( T.NAME  _) = nm
  constFoldExp _ tmp@(T.TEMP  _) = tmp
  constFoldExp constMap (T.BINOP (op, e1, e2)) =
    case T.BINOP (op, constFoldExp constMap e1, constFoldExp constMap e2) of
      T.BINOP (op', T.CONST c1, T.CONST c2) -> T.CONST $ (convert op') c1 c2
      binop@_                               -> binop
  constFoldExp constMap (T.MEM e) = T.MEM $ constFoldExp constMap e
  constFoldExp constMap (T.CALL (f, args, escapes, hasResult)) = T.CALL
    ( constFoldExp constMap f
    , fmap (constFoldExp constMap) args
    , escapes
    , hasResult
    )
  constFoldExp constMap (T.CALLNORETURN (f, args, escapes)) = T.CALLNORETURN
    (constFoldExp constMap f, fmap (constFoldExp constMap) args, escapes)
  constFoldExp constMap (T.ESEQ (stm, e)) =
    T.ESEQ (stm, constFoldExp constMap e)

  convert T.PLUS    = (+)
  convert T.MINUS   = (-)
  convert T.MUL     = (*)
  convert T.DIV     = div
  convert T.AND     = \arg1 arg2 -> fromEnum $ arg1 /= 0 && arg2 /= 0
  convert T.OR      = \arg1 arg2 -> fromEnum $ arg1 /= 0 || arg2 /= 0
  convert T.LSHIFT  = shiftL
  convert T.RSHIFT  = shiftR
  convert T.ARSHIFT = error "don't know how to convert ARSHIFT"
  convert T.XOR     = xor

propagateConstants :: Canon.Block -> Canon.Block
propagateConstants bb = evalState (mapM propagateConstantsM bb) Map.empty
 where
  propagateConstantsM :: T.Stm -> State (Map.Map Int Int) T.Stm
  propagateConstantsM stm@(T.MOVE (T.TEMP dst, T.TEMP src)) = do
    constMap <- get
    case Map.lookup src constMap of
      Just constVal ->
        let constMap' = Map.insert dst constVal constMap
        in  do
              put constMap'
              pure $ T.MOVE (T.TEMP dst, T.CONST constVal)
      _ -> do
        deleteFromConstMap stm
        pure stm
  propagateConstantsM stm@(T.MOVE (T.TEMP dst, T.CONST constVal)) = do
    constMap <- get
    let constMap' = Map.insert dst constVal constMap
    put constMap'
    pure stm
  propagateConstantsM stm@(T.MOVE (tmp@(T.TEMP _), e)) = do
    deleteFromConstMap stm
    constMap <- get
    pure $ T.MOVE (tmp, constPropExp constMap e)
  propagateConstantsM (T.MOVE (e1, e2)) = do
    constMap <- get
    pure $ T.MOVE (e1, constPropExp constMap e2)
  propagateConstantsM (T.EXP e) = do
    constMap <- get
    pure $ T.EXP $ constPropExp constMap e
  propagateConstantsM j@( T.JUMP  _) = pure j
  propagateConstantsM cj@(T.CJUMP _) = do
    constMap <- get
    pure $ constPropStm constMap cj
  propagateConstantsM s@(T.SEQ _) = do
    constMap <- get
    pure $ constPropStm constMap s
  propagateConstantsM lab@(T.LABEL _) = pure lab

  constPropExp :: Map.Map Int Int -> T.Exp -> T.Exp
  constPropExp _        c@(T.CONST _) = c
  constPropExp _        t@(T.NAME  _) = t
  constPropExp constMap e@(T.TEMP  i) = case Map.lookup i constMap of
    Just constVal -> T.CONST constVal
    _             -> e
  constPropExp constMap (T.BINOP (op, e1, e2)) =
    T.BINOP (op, constPropExp constMap e1, constPropExp constMap e2)
  constPropExp constMap (T.MEM e) = T.MEM $ constPropExp constMap e
  constPropExp constMap (T.CALL (f, args, escapes, hasResult)) = T.CALL
    ( constPropExp constMap f
    , fmap (constPropExp constMap) args
    , escapes
    , hasResult
    )
  constPropExp constMap (T.CALLNORETURN (f, args, escapes)) = T.CALLNORETURN
    (constPropExp constMap f, fmap (constPropExp constMap) args, escapes)
  constPropExp constMap (T.ESEQ (stm, e)) =
    T.ESEQ (constPropStm constMap stm, constPropExp constMap e)

  constPropStm :: Map.Map Int Int -> T.Stm -> T.Stm
  constPropStm constMap (T.MOVE (e1, e2)) =
    T.MOVE (e1, constPropExp constMap e2)
  constPropStm constMap (T.EXP e) = T.EXP $ constPropExp constMap e
  constPropStm constMap (T.JUMP (e, labs)) =
    T.JUMP (constPropExp constMap e, labs)
  constPropStm constMap (T.CJUMP (op, e1, e2, lab1, lab2)) =
    T.CJUMP (op, constPropExp constMap e1, constPropExp constMap e2, lab1, lab2)
  constPropStm constMap (T.SEQ (stm1, stm2)) =
    T.SEQ (constPropStm constMap stm1, constPropStm constMap stm2)
  constPropStm _ lab@(T.LABEL _) = lab

deleteFromConstMap :: T.Stm -> State (Map.Map Int Int) ()
deleteFromConstMap (T.MOVE (T.TEMP dst, _)) = do
  constMap <- get
  let constMap' = Map.delete dst constMap
  put constMap'
  pure ()
deleteFromConstMap _ = pure ()
