module Eval(eval) where

import Syntax

eval :: Expr -> Env -> Value
eval (ELit (LInt n))  _ = VInt n
eval (ELit (LBool b)) _ = VBool b
eval (EOp op e1 e2) env = binop op e1 e2 env
eval (EVar x) env =
    case lookupEnv x env of
      VThunk (EFix f e) env' -> eval (EFix f e) env'
      v                      -> v
eval (ELam x e) env = VClos x e env
eval (EApp e1 e2) env = v2 `seq` eval e3 ((x, v2):env')
    where
      VClos x e3 env' = eval e1 env
      v2              = eval e2 env
eval (ELet x e1 e2) env = eval e2 env'
    where
      v    = eval e1 env
      env' = env `ext` (x,v)
eval (EFix f e) env = eval e env'
    where env' = env `ext` (f, (VThunk (EFix f e) env))
eval (EIf e1 e2 e3) env = if b then eval e2 env else eval e3 env
    where VBool b = eval e1 env

binop :: BinOp -> Expr -> Expr -> Env -> Value
binop op e1 e2 env =
    case (op, eval e1 env, eval e2 env) of
      (Add, VInt n1,  VInt n2)  -> VInt $ n1 + n2
      (Sub, VInt n1,  VInt n2)  -> VInt $ n1 - n2
      (Mul, VInt n1,  VInt n2)  -> VInt $ n1 * n2
      (And, VBool b1, VBool b2) -> VBool $ b1 && b2
      (Or,  VBool b1, VBool b2) -> VBool $ b1 || b2
      (Eq,  VInt n1,  VInt n2)  -> VBool $ n1 == n2
      (Eq,  VBool b1, VBool b2) -> VBool $ b1 == b2
      (Gt,  VInt n1,  VInt n2)  -> VBool $ n1 > n2
      (Lt,  VInt n1,  VInt n2)  -> VBool $ n1 < n2
      (_,_,_) -> error $ "FATAL ERROR: " ++ show op ++ ":" ++ show e1 ++ ", " ++ show e2
      -- Last one should be unreachable when the expression is typed.
