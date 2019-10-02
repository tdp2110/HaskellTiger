module Canon where

import qualified Temp
import qualified Tree as T

import Control.Monad.Trans.State (State, get, put, runState)

(%) :: T.Stm -> T.Stm -> T.Stm
(%) (T.EXP (T.CONST _)) x = x
(%) x (T.EXP (T.CONST _)) = x
(%) x y = T.SEQ (x, y)

commute :: T.Stm -> T.Exp -> Bool
commute (T.EXP (T.CONST _)) _ = True
commute _ (T.NAME _) = True
commute _ (T.CONST _) = True
commute _ _ = False

nop :: T.Stm
nop = T.EXP $ T.CONST 0

newTemp :: State Temp.Generator Int
newTemp = do
  gen <- get
  let
    (t, gen') = Temp.newtemp gen
    in
    do
      put gen'
      pure t

newLabel :: State Temp.Generator Temp.Label
newLabel = do
  gen <- get
  let
    (lab, gen') = Temp.newlabel gen
    in
    do
      put gen'
      pure lab

reorder :: [T.Exp] -> State Temp.Generator (T.Stm, [T.Exp])
reorder (e@(T.CALL _):rest) = do
    t <- newTemp
    reorder $ (T.ESEQ (T.MOVE (T.TEMP t, e), T.TEMP t)) : rest
reorder (a:rest) = do
  (stms,e) <- doExp a
  (stms',el) <- reorder rest
  if commute stms' e then
    pure (stms % stms',e:el)
    else
    do
      t <- newTemp
      pure (stms % (T.MOVE (T.TEMP t, e)) % stms', (T.TEMP t) : el)
reorder [] = do
  pure (nop,[])

reorderExp :: [T.Exp] -> ([T.Exp] -> T.Exp) -> State Temp.Generator (T.Stm, T.Exp)
reorderExp el build = do
  (stms,el') <- reorder el
  pure (stms, build el')

reorderStm :: [T.Exp] -> ([T.Exp] -> T.Stm) -> State Temp.Generator T.Stm
reorderStm el build = do
  (stms,el') <- reorder el
  pure $ stms % build el'

doStm :: T.Stm -> State Temp.Generator T.Stm
doStm (T.SEQ (a,b)) = do
  stmA <- doStm a
  stmB <- doStm b
  pure $ stmA % stmB
doStm (T.JUMP (e,labs)) =
  reorderStm [e] (\[e'] -> T.JUMP (e', labs))
doStm (T.CJUMP (p,a,b,t,f)) =
  reorderStm [a,b] $ \[a',b'] -> T.CJUMP (p,a',b',t,f)
doStm (T.MOVE (T.TEMP t, T.CALL (e,el))) =
  reorderStm (e:el) $ \(e':el') -> T.MOVE (T.TEMP t, T.CALL (e',el'))
doStm (T.MOVE (T.TEMP t, b)) =
  reorderStm [b] $ \[b'] -> T.MOVE (T.TEMP t, b')
doStm (T.MOVE (T.MEM e, b)) =
  reorderStm [e,b] $ \[e',b'] -> T.MOVE (T.MEM e', b')
doStm (T.MOVE (T.ESEQ (s,e), b)) =
  doStm $ T.SEQ (s, T.MOVE (e,b))
doStm (T.EXP (T.CALL (e,el))) =
  reorderStm (e:el) $ \(e':el') -> T.EXP $ T.CALL (e',el')
doStm (T.EXP e) =
  reorderStm [e] $ \[e'] -> T.EXP e'
doStm s =
  reorderStm [] $ \[] -> s

doExp :: T.Exp -> State Temp.Generator (T.Stm, T.Exp)
doExp (T.BINOP (p,a,b)) =
  reorderExp [a,b] $ \[a',b'] -> T.BINOP (p,a',b')
doExp (T.MEM a) =
  reorderExp [a] $ \[a'] -> T.MEM a'
doExp (T.ESEQ (s,e)) = do
  stms <- doStm s
  (stms',e') <- doExp e
  pure (stms % stms',e')
doExp (T.CALL (e,el)) =
  reorderExp (e:el) (\(e':el') -> T.CALL (e', el'))
doExp e =
  reorderExp [] $ \[] -> e

linear :: T.Stm -> [T.Stm] -> State Temp.Generator [T.Stm]
linear (T.SEQ (a,b)) l = do
  stms <- linear b l
  linear a stms
linear s l = do
  pure (s:l)

linearizeM :: T.Stm -> State Temp.Generator [T.Stm]
linearizeM s = do
  s' <- doStm s
  linear s' []

{-
From an arbitrary Tree statement, produce a list of cleaned satisfying the following properties:
  1.  No SEQ's or ESEQ's
  2.  The parent of every CALL is an EXP(..) or a MOVE(TEMP t,..)
-}
linearize :: T.Stm -> Temp.Generator -> ([T.Stm], Temp.Generator)
linearize s gen =
  runState (linearizeM s) gen

type Block = [T.Stm]

basicBlocks :: [T.Stm] -> State Temp.Generator ([Block], Temp.Label)
basicBlocks stms = do
  done <- newLabel
  let
    blocks :: [T.Stm] -> [Block] -> State Temp.Generator [Block]
    blocks (labStm@(T.LABEL _) : stms') blist =
      let
        next :: [T.Stm] -> Block -> State Temp.Generator [Block]
        next (s@(T.JUMP _) : rest) thisBlock =
          endBlock rest (s : thisBlock)
        next (s@(T.CJUMP _) : rest) thisBlock =
          endBlock rest (s : thisBlock)
        next stms''@(T.LABEL lab : _) thisBlock =
          next ((T.JUMP (T.NAME lab, [lab])) : stms'') thisBlock
        next (s : rest) thisBlock =
          next rest (s : thisBlock)
        next [] thisBlock =
          next [T.JUMP (T.NAME done, [done])] thisBlock

        endBlock :: [T.Stm] -> Block -> State Temp.Generator [Block]
        endBlock stms'' thisBlock =
          blocks stms'' $ reverse thisBlock : blist
      in
        next stms' [labStm]
    blocks [] blist = do
      pure $ reverse blist
    blocks stms' blist = do
      t <- newLabel
      blocks ((T.LABEL t) : stms') blist
    in
    do
      res <- blocks stms []
      pure (res, done)
