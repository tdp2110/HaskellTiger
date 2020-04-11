import Test.QuickCheck

import qualified Canon as C
import qualified Frame
import Symbol
import qualified Temp
import qualified TreeIR as T


instance Arbitrary Temp.Label where
  arbitrary = do
    n <- choose (0, 1024) :: Gen Int
    pure $ Temp.Label $ Symbol $ "L" ++ show n

instance Arbitrary T.Exp where
  arbitrary = sized arbExp

arbTemp :: Gen T.Exp
arbTemp = do
  i <- arbitrary
  pure $ T.TEMP i

arbMem :: Gen T.Exp
arbMem = do
  n <- choose (0, 8) :: Gen Int
  e <- arbExp n
  pure $ T.MEM e

arbExp :: Int -> Gen T.Exp
arbExp 0 = do
  n <- choose (0, 4) :: Gen Int
  case n of
    0 -> do
      i <- arbitrary
      pure $ T.CONST i
    1 -> do
      lab <- arbitrary
      pure $ T.NAME lab
    _ -> arbTemp
arbExp sz =
  let
    sz' = sz `div` 2
    expGen = arbExp sz'
    stmGen = arbStm sz'
  in do
    n <- choose (0, 7) :: Gen Int
    case n of
      0 -> do
        i <- choose (0, 500)
        pure $ T.CONST i
      1 -> do
        lab <- arbitrary
        pure $ T.NAME lab
      2 -> do
        i <- choose (0, 1000000)
        pure $ T.TEMP i
      3 -> do
        op <- arbitrary
        e1 <- expGen
        e2 <- expGen
        pure $ T.BINOP (op, e1, e2)
      4 -> arbMem
      5 -> do
        funcExp <- expGen
        argSize <- choose (0, 2) :: Gen Int
        args <- vectorOf argSize expGen
        hasResult <- arbitrary :: Gen Bool
        pure $ T.CALL (funcExp, args, fmap (\_ -> Frame.DoesNotEscape) args, hasResult)
      _ -> do
        s <- stmGen
        e <- expGen
        pure $ T.ESEQ (s, e)

instance Arbitrary T.Stm where
  arbitrary = sized arbStm

tempOrMem :: Gen T.Exp
tempOrMem = do
  n <- choose (0, 1) :: Gen Int
  case n of
    0 -> arbTemp
    _ -> arbMem

arbStm :: Int -> Gen T.Stm
arbStm 0 = do
  lab <- arbitrary
  pure $ T.LABEL lab
arbStm sz =
  let
    sz' = sz `div` 2
    expGen = arbExp sz'
    stmGen = arbStm sz'
  in do
    n <- choose (0, 6) :: Gen Int
    case n of
      0 -> do
        e1 <- tempOrMem
        e2 <- expGen
        pure $ T.MOVE (e1, e2)
      1 -> do
        e <- expGen
        pure $ T.EXP e
      2 -> do
        e <- expGen
        labsSize <- choose (0, 10) :: Gen Int
        labs <- vectorOf labsSize arbitrary
        pure $ T.JUMP (e, labs)
      3 -> do
        op <- arbitrary
        e1 <- expGen
        e2 <- expGen
        t <- arbitrary
        f <- arbitrary
        pure $ T.CJUMP (op, e1, e2, t, f)
      4 -> do
        s1 <- stmGen
        s2 <- stmGen
        pure $ T.SEQ (s1, s2)
      _ -> do
        lab <- arbitrary
        pure $ T.LABEL lab

instance Arbitrary T.Binop where
  arbitrary = elements [ T.PLUS
                       , T.MINUS
                       , T.MUL
                       , T.DIV
                       , T.AND
                       , T.OR
                       , T.LSHIFT
                       , T.RSHIFT
                       , T.ARSHIFT
                       , T.XOR]

instance Arbitrary T.Relop where
  arbitrary = elements [ T.EQ
                       , T.NE
                       , T.LT
                       , T.GT
                       , T.LE
                       , T.GE
                       , T.LE
                       , T.GE
                       , T.ULT
                       , T.ULE
                       , T.UGT
                       , T.UGE ]

gen :: Temp.Generator
gen = Temp.newGen

prop_canonHasNoSeq :: T.Stm -> Bool
prop_canonHasNoSeq stm =
  let
    (stms, _) = C.linearize stm gen
  in
    all stmHasNoSeq stms
  where
    stmHasNoSeq :: T.Stm -> Bool
    stmHasNoSeq (T.MOVE (e1, e2)) = expHasNoSeq e1 && expHasNoSeq e2
    stmHasNoSeq (T.EXP e) = expHasNoSeq e
    stmHasNoSeq (T.JUMP (e, _)) = expHasNoSeq e
    stmHasNoSeq (T.CJUMP (_, e1, e2, _, _)) = expHasNoSeq e1 && expHasNoSeq e2
    stmHasNoSeq (T.SEQ _) = False
    stmHasNoSeq _ = True

    expHasNoSeq :: T.Exp -> Bool
    expHasNoSeq (T.BINOP (_, e1, e2)) = expHasNoSeq e1 && expHasNoSeq e2
    expHasNoSeq (T.MEM e) = expHasNoSeq e
    expHasNoSeq (T.CALL (e, es, _, _)) = expHasNoSeq e && (all expHasNoSeq es)
    expHasNoSeq (T.ESEQ (_, _)) = False
    expHasNoSeq _ = True

prop_parentOfCallIsOk :: T.Stm -> Bool
prop_parentOfCallIsOk stm =
  let
    (stms, _) = C.linearize stm gen
  in
    all callParentOkStm stms
  where
    callParentOkExp :: T.Exp -> Bool
    callParentOkExp (T.BINOP (_, T.CALL _, _)) = False
    callParentOkExp (T.BINOP (_, _, T.CALL _)) = False
    callParentOkExp (T.MEM (T.CALL _)) = False
    callParentOkExp c@(T.CALL _) = callOk c
    callParentOkExp (T.ESEQ (s, e)) =
      callParentOkStm s && callParentOkExp e
    callParentOkExp _ = True

    callParentOkStm :: T.Stm -> Bool
    callParentOkStm (T.MOVE (T.TEMP _, c@(T.CALL _))) = callOk c
    callParentOkStm (T.MOVE (_, T.CALL _)) = False
    callParentOkStm (T.MOVE (T.CALL _, _)) = False
    callParentOkStm (T.EXP c@(T.CALL _)) = callOk c
    callParentOkStm (T.JUMP (T.CALL _, _)) = False
    callParentOkStm (T.CJUMP (_, (T.CALL _), _, _, _)) = False
    callParentOkStm (T.CJUMP (_, _, (T.CALL _), _, _)) = False
    callParentOkStm (T.SEQ (s1, s2)) = callParentOkStm s1 && callParentOkStm s2
    callParentOkStm _ = True

    callOk :: T.Exp -> Bool
    callOk (T.CALL (f, args, _, _)) =
      callParentOkExp f && all callParentOkExp args
    callOk _ = error "shouldn't get here"

main :: IO ()
main = do
  quickCheck prop_canonHasNoSeq
  quickCheck prop_parentOfCallIsOk
