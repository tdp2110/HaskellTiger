import Data.List
--import Control.Monad
--import Control.Monad.Trans.Writer.Strict

type Id = [Char]

data BinOp = Plus | Minus | Times | Div deriving Show

data Stm = CompountStm Stm Stm
         | AssignStm Id Exp
         | PrintStm [Exp] deriving Show
data Exp = IdExp Id
         | NumExp Integer
         | OpExp Exp BinOp Exp
         | EseqExp Stm Exp deriving Show

-- problem 1
maxargs :: Stm -> Integer
maxargs (CompountStm stm1 stm2) = max (maxargs stm1) (maxargs stm2)
maxargs (AssignStm _ expr) = maxargsExp expr
maxargs (PrintStm exprs) = max (genericLength exprs) (maximum $ map maxargsExp exprs)

maxargsExp :: Exp -> Integer
maxargsExp (OpExp expr1 _ expr2) = max (maxargsExp expr1) (maxargsExp expr2)
maxargsExp (EseqExp stm expr) = max (maxargs stm) (maxargsExp expr)
maxargsExp _ = 0

-- problem 2

--interp :: Stm -> [[Char]]

type Binding = (Id, Integer)
type Env = [Binding]

push :: Binding -> Env -> Env
push binding env = [binding] ++ env

eval :: Exp -> Env -> Maybe Integer
eval (IdExp identifier) env = lookup identifier env
eval (NumExp n) _ = Just n
eval (OpExp e1 op e2) env = do
  v1 <- eval e1 env
  v2 <- eval e2 env
  let res = case op of
        Plus -> v1 + v2
        Minus -> v1 - v2
        Times -> v1 * v2
        Div -> div v1  v2
    in return res

prog1 :: Exp
prog2 :: Stm
prog1 = OpExp (NumExp 1337) Minus (IdExp "var")
prog2 = CompountStm (
  AssignStm "a" (OpExp (NumExp 5) Plus (NumExp 3)))
        (PrintStm [IdExp "b"])

main::IO()
main = do
  putStrLn "hello world"
  putStrLn $ show prog1
  putStrLn $ show prog2
  putStrLn $ show $ maxargs prog2
