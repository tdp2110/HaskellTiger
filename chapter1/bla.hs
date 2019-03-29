type Id = [Char]

data BinOp = Plus | Minus | Times | Div deriving Show

data Stm = CompountStm Stm Stm
         | AssignStm Id Exp
         | PrintStm [Exp] deriving Show
data Exp = IdExp Id
         | NumExp Int
         | OpExp Exp BinOp Exp
         | EseqExp Stm Exp deriving Show

prog1 = OpExp (NumExp 1337) Minus (IdExp "var")
prog2 = CompountStm (
  AssignStm "a" (OpExp (NumExp 5) Plus (NumExp 3)))
        (PrintStm [IdExp "b"])

main::IO()
main = do
  putStrLn "hello world"
  putStrLn $ show prog1
  putStrLn $ show prog2
