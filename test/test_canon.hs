import qualified Canon as C
import qualified Frame
import Symbol
import qualified Temp
import qualified TreeIR as T

import Test.QuickCheck
import Test.Hspec


instance Arbitrary Temp.Label where
  arbitrary = do
    n <- choose (0, 1024) :: Gen Int
    pure $ Temp.Label $ Symbol $ "L" ++ show n

instance Arbitrary T.Exp where
  arbitrary = sized arbExp

arbTemp :: Gen T.Exp
arbTemp = T.TEMP <$> arbitrary

arbMem :: Gen T.Exp
arbMem = do
  n <- choose (0, 32) :: Gen Int
  e <- arbExp n
  pure $ T.MEM e

arbExp :: Int -> Gen T.Exp
arbExp 0 = do
  n <- choose (0, 4) :: Gen Int
  case n of
    0 -> T.CONST <$> arbitrary
    1 -> T.NAME <$> arbitrary
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
      1 -> T.NAME <$> arbitrary
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
        pure $ T.CALL (funcExp, args, fmap (const Frame.DoesNotEscape) args, hasResult)
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
arbStm 0 = T.LABEL <$> arbitrary
arbStm sz =
  let
    sz' = sz `div` 2
    expGen = arbExp sz'
    stmGen = arbStm sz'
  in do
    n <- choose (0, 5) :: Gen Int
    case n of
      0 -> do
        e1 <- tempOrMem
        e2 <- expGen
        pure $ T.MOVE (e1, e2)
      1 -> T.EXP <$> expGen
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
      5 -> T.LABEL <$> arbitrary
      _ -> error "shouldn't get here"

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
    expHasNoSeq (T.CALL (e, es, _, _)) = expHasNoSeq e && all expHasNoSeq es
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
    callParentOkExp (T.CONST _) = True
    callParentOkExp (T.NAME _) = True
    callParentOkExp (T.TEMP _) = True
    callParentOkExp (T.BINOP (_, T.CALL _, _)) = False
    callParentOkExp (T.BINOP (_, _, T.CALL _)) = False
    callParentOkExp (T.BINOP (_, e1, e2)) = callParentOkExp e1 && callParentOkExp e2
    callParentOkExp (T.MEM (T.CALL _)) = False
    callParentOkExp (T.MEM e) = callParentOkExp e
    callParentOkExp (T.CALL (f, args, _, _)) = callArgsOk f args
    callParentOkExp (T.CALLNORETURN (f, args, _)) = callArgsOk f args
    callParentOkExp (T.ESEQ (_, T.CALL _)) = False
    callParentOkExp (T.ESEQ (s, e)) = callParentOkStm s && callParentOkExp e

    callParentOkStm :: T.Stm -> Bool
    callParentOkStm (T.MOVE (T.TEMP _, e)) = callParentOkExp e
    callParentOkStm (T.MOVE (_, T.CALL _)) = False
    callParentOkStm (T.MOVE (e1, e2)) = callParentOkExp e1 && callParentOkExp e2
    callParentOkStm (T.EXP e) = callParentOkExp e
    callParentOkStm (T.JUMP (T.CALL _, _)) = False
    callParentOkStm (T.JUMP (e, _)) = callParentOkExp e
    callParentOkStm (T.CJUMP (_, T.CALL _, _, _, _)) = False
    callParentOkStm (T.CJUMP (_, _, T.CALL _, _, _)) = False
    callParentOkStm (T.CJUMP (_, e1, e2, _, _)) = callParentOkExp e1 && callParentOkExp e2
    callParentOkStm (T.SEQ (s1, s2)) = callParentOkStm s1 && callParentOkStm s2
    callParentOkStm (T.LABEL _) = True

    callArgsOk f args =
      not (any isCall (f:args)) && all callParentOkExp (f:args)

    isCall (T.CALL _) = True
    isCall _ = False

main :: IO ()
main = hspec $ do
  describe "linearize" $
    it "has no SEQ nor ESEQ" $
      property prop_canonHasNoSeq
  describe "linearize" $
    it "parent of every CALL is an EXP(..) or a MOVE(TEMP t,..)" $
      property prop_parentOfCallIsOk
