import Data.List

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
data Error = DivByZero | UnboundVar Id Env deriving Show
data StopState = Ok | Fail Error deriving Show

interpStm :: Stm -> Env -> ([[Char]], Env, StopState)
interpStm (CompoundStm stm1 stm2) env =
  let res@(log1, env1, state1) = interpStm stm1 env in
    case state1 of
      Fail _ -> res
      Ok -> let (log2, env2, state2) = interpStm stm2 env1 in
              (log1 ++ log2, env2, state2)
interpStm (AssignStm identifier expr) env =
  let (log, env', errorOrVal) = interpExp expr env in
    case errorOrVal of
      Left error -> (log, env', Fail error)
      Right val -> (log, [(identifier, val)] ++ env', Ok)
interpStm (PrintStm exprs) env =
  let accum res@(logAccum, envAccum, stateAccum) expr =
        case stateAccum of
          Fail _ -> res
          Ok -> let (nextLog, nextEnv, errorOrVal) = interpExp expr envAccum in
            case errorOrVal of
              Left error -> (logAccum ++ nextLog, nextEnv, Fail error)
              Right val -> (logAccum ++ nextLog ++ [show val], nextEnv, Ok)
  in foldl' accum ([], env, Ok) exprs

lookup' :: Id -> Env -> Either Error Integer
lookup' id env = case lookup id env of
  Nothing -> Left $ UnboundVar id env
  Just v -> Right v

interpExp :: Exp -> Env -> ([[Char]], Env, Either Error Integer)
interpExp (IdExp identifier) env = ([], env, lookup' identifier env)
interpExp (NumExp n) env= ([], env, Right n)
interpExp (OpExp e1 op e2) env =
  let res@(log1, env1, errOrV1) = interpExp e1 env in
    case errOrV1 of
      Left _ -> res
      Right v1 -> let (log2, env2, errOrV2) = interpExp e2 env1 in
        let errOrResult = do
            v2 <- errOrV2
            case op of
              Plus -> return $ v1 + v2
              Minus -> return $ v1 - v2
              Times -> return $ v1 * v2
              Div -> case v2 of
                       0 -> Left DivByZero
                       _ -> return $ div v1 v2
        in (log1 ++ log2, env2 ++ env1, errOrResult)
interpExp (EseqExp stm e) env=
  let (log', env', okOrErr) = interpStm stm env in
    case okOrErr of
      Fail err -> (log', env', Left err)
      Ok -> let (log'', env'', res) = interpExp e env' in
        (log' ++ log'', env'', res)

prog1 :: Exp
prog2 :: Stm
prog1 = OpExp (NumExp 1337) Minus (IdExp "var")
prog2 = CompoundStm (
  AssignStm "a" (OpExp (NumExp 5) Plus (NumExp 3)))
        (PrintStm [IdExp "b"])

res1 = interpStm (CompoundStm (CompoundStm (AssignStm "var" $ NumExp 42) (AssignStm "x" prog1)) (PrintStm [IdExp "var", IdExp "x"])) []

res2 = interpStm (PrintStm [IdExp "var", OpExp (NumExp 1) Div (IdExp "var")]) [("var", 0)]

main::IO()
main = do
  putStrLn "hello world"
  putStrLn $ show prog1
  putStrLn $ show prog2
  putStrLn $ show $ maxargs prog2
  putStrLn $ show res1
  putStrLn $ show res2
