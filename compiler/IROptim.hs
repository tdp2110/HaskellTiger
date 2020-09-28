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
                                                , (.&.)
                                                , (.|.)
                                                )


optimizeBasicBlock :: Canon.Block -> Canon.Block
optimizeBasicBlock bb =
  let bb' = (foldConstants . propagateConstants . simplifyStms) bb
  in  if bb' == bb then bb else optimizeBasicBlock bb'

simplifyStms :: Canon.Block -> Canon.Block
simplifyStms = fmap simplifyStm
 where
  simplifyStm :: T.Stm -> T.Stm
  simplifyStm (T.MOVE (e1, e2) ) = T.MOVE (simplifyExp e1, simplifyExp e2)
  simplifyStm (T.EXP  e        ) = T.EXP $ simplifyExp e
  simplifyStm (T.JUMP (e, labs)) = T.JUMP (simplifyExp e, labs)
  simplifyStm (T.CJUMP (op, e1, e2, lab1, lab2)) =
    let e1' = simplifyExp e1
        e2' = simplifyExp e2
        j   = T.CJUMP (op, e1', e2', lab1, lab2)
    in  if isPureExp e1' && isPureExp e2' && e1 == e2
          then
            let trueJump  = T.JUMP (T.NAME lab1, [lab1])
                falseJump = T.JUMP (T.NAME lab2, [lab2])
            in  case op of
                  T.EQ -> trueJump
                  T.NE -> falseJump
                  T.LT -> falseJump
                  T.GT -> falseJump
                  T.LE -> trueJump
                  T.GE -> trueJump
                  _    -> j
          else j
  simplifyStm (T.SEQ (s1, s2)) =
    let s1'  = simplifyStm s1
        s2'  = simplifyStm s2
        seq' = T.SEQ (s1', s2')
    in  if isPureStm s1' then s2 else seq'
  simplifyStm lab@(T.LABEL _) = lab

  simplifyExp :: T.Exp -> T.Exp
  simplifyExp (T.BINOP (T.DIV  , e        , T.CONST 1)) = e
  simplifyExp (T.BINOP (T.MUL  , _        , T.CONST 0)) = T.CONST 0
  simplifyExp (T.BINOP (T.MUL  , T.CONST 0, _        )) = T.CONST 0
  simplifyExp (T.BINOP (T.MUL  , e        , T.CONST 1)) = e
  simplifyExp (T.BINOP (T.MUL  , T.CONST 1, e        )) = e
  simplifyExp (T.BINOP (T.PLUS , e        , T.CONST 0)) = e
  simplifyExp (T.BINOP (T.PLUS , T.CONST 0, e        )) = e
  simplifyExp (T.BINOP (T.MINUS, e        , T.CONST 0)) = e
  simplifyExp sub@(T.BINOP (T.MINUS, e1, e2)) =
    if isPureExp e1 && isPureExp e2 && e1 == e2 then T.CONST 0 else sub
  simplifyExp (T.MEM e) = T.MEM $ simplifyExp e
  simplifyExp (T.CALL (func, args, escapes, hasResult)) =
    T.CALL (simplifyExp func, fmap simplifyExp args, escapes, hasResult)
  simplifyExp (T.CALLNORETURN (func, args, escapes)) =
    T.CALLNORETURN (simplifyExp func, fmap simplifyExp args, escapes)
  simplifyExp (T.ESEQ (s, e)) =
    let s'    = simplifyStm s
        e'    = simplifyExp e
        eseq' = T.ESEQ (s', e')
    in  if isPureStm s' then e' else eseq'
  simplifyExp e = e

  isPureExp :: T.Exp -> Bool
  isPureExp e = case e of
    T.CONST        _           -> True
    T.NAME         _           -> True
    T.TEMP         _           -> True
    T.BINOP        (_, e1, e2) -> isPureExp e1 && isPureExp e2
    T.MEM          expr        -> isPureExp expr
    T.CALL         _           -> False -- overly conservative
    T.CALLNORETURN _           -> False -- overly conservative
    T.ESEQ         (stm, expr) -> isPureStm stm && isPureExp expr

  isPureStm :: T.Stm -> Bool
  isPureStm s = case s of
    T.MOVE  (T.TEMP _, _)     -> False
    T.MOVE  (T.MEM  _, _)     -> False
    T.MOVE  (_       , _)     -> error "malformed move exp"
    T.EXP   e                 -> isPureExp e
    T.JUMP  (e, _)            -> isPureExp e
    T.CJUMP (_, e1, e2, _, _) -> isPureExp e1 && isPureExp e2
    T.SEQ   (s1, s2)          -> isPureStm s1 && isPureStm s2
    T.LABEL _                 -> True

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
  constFoldExp constMap binop@(T.BINOP (op, e1, e2)) =
    case (constFoldExp constMap e1, constFoldExp constMap e2) of
      (T.CONST c1, T.CONST c2) -> T.CONST $ convertOp op c1 c2
      _                        -> binop
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

  convertOp T.PLUS    = (+)
  convertOp T.MINUS   = (-)
  convertOp T.MUL     = (*)
  convertOp T.DIV     = div
  convertOp T.MOD     = mod
  convertOp T.AND     = (.&.)
  convertOp T.OR      = (.|.)
  convertOp T.LSHIFT  = shiftL
  convertOp T.RSHIFT  = shiftR
  convertOp T.ARSHIFT = error "don't know how to convert ARSHIFT"
  convertOp T.XOR     = xor

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
  propagateConstantsM j@(T.JUMP  _                       ) = pure j
  propagateConstantsM (  T.CJUMP (op, e1, e2, lab1, lab2)) = do
    constMap <- get
    let e1' = constPropExp constMap e1
    let e2' = constPropExp constMap e2
    pure $ case (e1', e2') of
      (T.CONST c1, T.CONST c2) -> if evalOp op c1 c2
        then T.JUMP (T.NAME lab1, [lab1])
        else T.JUMP (T.NAME lab2, [lab2])
      _ -> T.CJUMP (op, e1', e2', lab1, lab2)
   where
    evalOp T.EQ  = (==)
    evalOp T.NE  = (/=)
    evalOp T.LT  = (<)
    evalOp T.GT  = (>)
    evalOp T.LE  = (<=)
    evalOp T.GE  = (>=)
    evalOp T.ULT = (<)
    evalOp T.ULE = (<=)
    evalOp T.UGT = (>)
    evalOp T.UGE = (>=)
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
