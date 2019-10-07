import Test.QuickCheck

import qualified Canon as C
import Symbol
import qualified Temp
import qualified Tree as T


instance Arbitrary Temp.Label where
  arbitrary = do
    n <- choose (0, 1024) :: Gen Int
    pure $ Temp.Label $ Symbol $ "L" ++ show n

instance Arbitrary T.Exp where
  arbitrary = do
    n <- choose (0, 6) :: Gen Int
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
        e1 <- arbitrary
        e2 <- arbitrary
        pure $ T.BINOP (op, e1, e2)
      4 -> do
        e <- arbitrary
        pure $ T.MEM e
      5 -> do
        funcExp <- arbitrary
        argSize <- choose (0, 2) :: Gen Int
        args <- vectorOf argSize arbitrary
        pure $ T.CALL (funcExp, args)
      _ -> do
        s <- arbitrary
        e <- arbitrary
        pure $ T.ESEQ (s, e)

instance Arbitrary T.Stm where
  arbitrary = do
    n <- choose (0, 6) :: Gen Int
    case n of
      0 -> do
        e1 <- arbitrary
        e2 <- arbitrary
        pure $ T.MOVE (e1, e2)
      1 -> do
        e <- arbitrary
        pure $ T.EXP e
      2 -> do
        e <- arbitrary
        labs <- listOf arbitrary
        pure $ T.JUMP (e, labs)
      3 -> do
        op <- arbitrary
        e1 <- arbitrary
        e2 <- arbitrary
        t <- arbitrary
        f <- arbitrary
        pure $ T.CJUMP (op, e1, e2, t, f)
      4 -> do
        s1 <- arbitrary
        s2 <- arbitrary
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
    expHasNoSeq (T.CALL (e, es)) = expHasNoSeq e && (all expHasNoSeq es)
    expHasNoSeq (T.ESEQ (s, e)) = stmHasNoSeq s && expHasNoSeq e
    expHasNoSeq _ = True

main :: IO ()
main = quickCheck prop_canonHasNoSeq
