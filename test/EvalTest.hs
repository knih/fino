module EvalTest where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Syntax
import Parser
import Eval

evalTests =
    [ "Eval Lit 1"   ~: eval (parseExpr "123") []
                         ~?= VInt 123
    , "Eval Lit 2"   ~: eval (parseExpr "True") []
                         ~?= VBool True
    , "Eval Lit 3"   ~: eval (parseExpr "False") []
                         ~?= VBool False
    , "Eval Lit 4"   ~: eval (parseExpr "x") [("x", VInt 1)]
                         ~?= VInt 1
    , "EVal binop 1" ~: eval (parseExpr "1 + 2") []
                         ~?= VInt 3
    , "Eval binop 2" ~: eval (parseExpr "1 - 2") []
                         ~?= VInt (-1)
    , "Eval binop 3" ~: eval (parseExpr "1 == 1") []
                         ~?= VBool True
    , "Eval binop 4" ~: eval (parseExpr "True == False") []
                         ~?= VBool False
    , "Eval let"     ~: eval (parseExpr "let x = 1 in x + 1") []
                         ~?= VInt 2
    , "Eval fun"     ~: eval (parseExpr "fun x -> x * x") []
                         ~?= VClos "x" (EOp Mul (EVar "x") (EVar "x")) []
    , "Eval app"     ~: eval (parseExpr "(fun x -> x) 1") []
                         ~?= VInt 1
    , "Eval fix 1"   ~: eval (parseExpr "fix f (fun x -> f x)") []
                         ~?= VClos "x" (EApp (EVar "f") (EVar "x"))
                                 [("f",VThunk (EFix "f" (ELam "x" (EApp (EVar "f") (EVar "x")))) [])]
    , "Eval fix 2"   ~: eval (parseExpr "let fib = fix f (fun n -> if n==0 || n==1 then 1 else f (n-1) + f (n-2)) in fib 10") []
                         ~?= VInt 89
    , "Eval if"      ~: eval (parseExpr "if True then 1 else 2") []
                         ~?= VInt 1
    ]
