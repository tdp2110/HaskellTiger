import Data.List
--import Control.Monad
--import Control.Monad.Trans.Writer.Strict

type Id = [Char]

data BinOp = Plus | Minus | Times | Div deriving Show

data Stm = CompoundStm Stm Stm
         | AssignStm Id Exp
         | PrintStm [Exp] deriving Show
data Exp = IdExp Id
         | NumExp Integer
         | OpExp Exp BinOp Exp
         | EseqExp Stm Exp deriving Show

-- problem 1
maxargs :: Stm -> Integer
maxargs (CompoundStm stm1 stm2) = max (maxargs stm1) (maxargs stm2)
maxargs (AssignStm _ expr) = maxargsExp expr
maxargs (PrintStm exprs) = max (genericLength exprs) (maximum $ map maxargsExp exprs)

maxargsExp :: Exp -> Integer
maxargsExp (OpExp expr1 _ expr2) = max (maxargsExp expr1) (maxargsExp expr2)
maxargsExp (EseqExp stm expr) = max (maxargs stm) (maxargsExp expr)
maxargsExp _ = 0

-- problem 2

type Binding = (Id, Integer)
type Env = [Binding]

interpStm :: Env -> Stm -> ([[Char]], Env)
interpStm env (CompoundStm stm1 stm2) =
  let (log, env') = interpStm env stm1 in
    interpStm env' stm2
interpStm env (AssignStm identifier expr) =
  let (log, env', maybeVal) = interpExp env expr in
    case maybeVal of
      Nothing -> error "couldn't evaluate"
      Just val -> (log, [(identifier, val)] ++ env')
interpStm env (PrintStm exprs) =
  let accum (logAccum, envAccum) expr =
        let (nextLog, nextEnv, maybeVal) = interpExp envAccum expr in
          case maybeVal of
            Nothing -> error "couldn't evaluate in PrintStm"
            Just v -> (logAccum ++ nextLog ++ [show v], nextEnv)
  in foldl' accum ([], env) exprs

interpExp :: Env -> Exp -> ([[Char]], Env, Maybe Integer)
interpExp env (IdExp identifier) = ([], env, lookup identifier env)
interpExp env (NumExp n) = ([], env, Just n)
interpExp env (OpExp e1 op e2) =
  let (log1, env1, maybeV1) = interpExp env e1 in
    let (log2, env2, maybeV2) = interpExp env1 e2 in
      let maybeVal = do
            v1 <- maybeV1
            v2 <- maybeV2
            let res = case op of
                  Plus -> v1 + v2
                  Minus -> v1 - v2
                  Times -> v1 * v2
                  Div -> div v1  v2 -- let's see if we can deal with div _ 0
              in return res
      in (log1 ++ log2, env2 ++ env1, maybeVal)
interpExp env (EseqExp stm e) =
  let (log, env') = interpStm env stm in
    let (log', env'', maybeVal) = interpExp env' e in
      (log ++ log', env'', maybeVal)

prog1 :: Exp
prog2 :: Stm
prog1 = OpExp (NumExp 1337) Minus (IdExp "var")
prog2 = CompoundStm (
  AssignStm "a" (OpExp (NumExp 5) Plus (NumExp 3)))
        (PrintStm [IdExp "b"])

(prints, _) = interpStm [] $ CompoundStm (CompoundStm (AssignStm "var" $ NumExp 42) (AssignStm "x" prog1)) (PrintStm [IdExp "var", IdExp "x"])

main::IO()
main = do
  putStrLn "hello world"
  putStrLn $ show prog1
  putStrLn $ show prog2
  putStrLn $ show $ maxargs prog2
  putStrLn $ show prints
