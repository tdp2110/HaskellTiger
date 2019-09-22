module Canon where

import qualified Temp
import qualified Tree as T


{-
From an arbitrary Tree statement, produce a list of cleaned satisfying the following properties:
  1.  No SEQ's or ESEQ's
  2.  The parent of every CALL is an EXP(..) or a MOVE(TEMP t,..)
-}
linearize :: T.Stm -> [T.Stm]
linearize = undefined

(%) :: T.Stm -> T.Stm -> T.Stm
(%) (T.EXP (T.CONST _)) x = x
(%) x (T.EXP (T.CONST _)) = x
(%) x y = T.SEQ (x, y)

commute :: T.Stm -> T.Exp -> Bool
commute (T.EXP (T.CONST _)) _ = True
commute _ (T.NAME _) = True
commute _ (T.CONST _) = True
commute _ _ = False
