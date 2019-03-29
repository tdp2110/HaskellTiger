type Id = [Char]

data BinOp = Plus | Minus | Times | Div deriving Show

data Stm = CompountStm Stm Stm
         | AssignStm Id Exp
         | PrintStm [Exp] deriving Show
data Exp = IdExp Id
         | NumExp Int
         | OpExp Exp BinOp Exp
         | EseqExp Stm Exp deriving Show

-- problem 1
maxargs :: Stm -> Int
maxargs (CompountStm stm1 stm2) = max (maxargs stm1) (maxargs stm2)
maxargs (AssignStm _ expr) = maxargsExp expr
maxargs (PrintStm exprs) = max (length exprs) (maximum $ map maxargsExp exprs)

maxargsExp :: Exp -> Int
maxargsExp (OpExp expr1 _ expr2) = max (maxargsExp expr1) (maxargsExp expr2)
maxargsExp (EseqExp stm expr) = max (maxargs stm) (maxargsExp expr)
maxargsExp _ = 0

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
