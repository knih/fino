module InferTest where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Syntax
import Parser
import Infer
    
support e tenv = subst s ρ
    where (s, _) = infer tenv e ρ 1
          ρ = TVar 0

inferTests =
    [ "Infer base 1"  ~: inferExpr (parseExpr "1")
                          ~?= TBase TInt
    , "Infer base 2"  ~: inferExpr (parseExpr "True")
                          ~?= TBase TBool
    , "Infer binop 1" ~: inferExpr (parseExpr "1 + 1 * 2")
                          ~?= TBase TInt
    , "Infer binop 2" ~: inferExpr (parseExpr "1 > 1 || 1 < 2 || 1 == 1 || True && False")
                          ~?= TBase TBool
    , "Infer var"     ~: support (parseExpr "x") [("x", Forall [] (TBase TInt))]
                          ~?= TBase TInt
    , "Infer lam"     ~: inferExpr (parseExpr "fun x -> x + 1")
                          ~?= TFun (TBase TInt) (TBase TInt)
    , "Infer app"     ~: inferExpr (parseExpr "(fun x -> x + 1) 2")
                          ~?= TBase TInt
    , "Infer let"     ~: inferExpr (parseExpr "let x = 1 in x + 1")
                          ~?= TBase TInt
    , "Infer fix"     ~: inferExpr (parseExpr "fix f (fun n -> if n==0 || n==1 then 1 else f (n-1) + f (n-2))")
                          ~?= TFun (TBase TInt) (TBase TInt)
    , "Infer if"      ~: inferExpr (parseExpr "if 1 == 1 then 1 else 2")
                          ~?= TBase TInt
    ] ++ poly

poly =
    [ "Infer poly 1" ~: let (TFun (TVar a1) (TVar a2)) = inferExpr (parseExpr "fun x -> x") in a1 == a2
                         ~?= True
    , "Infer poly 2" ~: let (TFun (TFun (TVar a1) (TVar a2)) (TFun (TVar b1) (TVar b2)))
                                = inferExpr (parseExpr "fun f -> fun g -> f g")
                        in a1 == b1 && a2 == b2
                         ~?= True
    , "Infer poly 3" ~: inferExpr (parseExpr "let id = fun x -> x in id 1 == 1 && id True == True")
                         ~?= TBase TBool
    , "Infer ploy 4" ~: let (TFun (TVar a1) (TVar a2)) = inferExpr (parseExpr "let id = fun x -> x in id id id")
                        in a1 == a2
                         ~?= True
    , "Infer poly 5" ~: let (TFun (TVar a1) (TVar a2))
                                = inferExpr (parseExpr "let id = fun x -> let id' = x in (fun y -> y) id' in id")
                        in a1 == a2
                         ~?= True
    , "Infer poly 6" ~: let (TFun (TFun (TBase TBool) (TVar a1)) (TVar a2))
                                = inferExpr (parseExpr "fun x -> let f = x in let y = f True in y")
                        in a1 == a2 
                         ~?= True
    ]
