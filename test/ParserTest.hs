module ParserTest where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Syntax
import Parser

expressions =
    [ "Parser Lit 1"    ~: parseExpr "True"
                            ~?= ELit (LBool True)
    , "Parser Lit 2"    ~: parseExpr "False"
                            ~?= ELit (LBool False)
    , "Parser Lit 3"    ~: parseExpr "0"
                            ~?= ELit (LInt 0)
    , "Parser Lit 4"    ~: parseExpr "9223372036854775807"
                            ~?= ELit (LInt 9223372036854775807)
    , "Parser Variable" ~: parseExpr "x"
                            ~?= (EVar "x")
    , "Parser Lambda"   ~: parseExpr "fun x -> x"
                            ~?= (ELam "x" (EVar "x"))
    , "Parser App"      ~: parseExpr "(fun x -> x) 1"
                            ~?= (EApp (ELam "x" (EVar "x")) (ELit (LInt 1)))
    , "Parser Let"      ~: parseExpr "let x = 1 in x"
                            ~?= (ELet "x" (ELit (LInt 1)) (EVar "x"))
    , "Parser Fix"      ~: parseExpr "fix f (fun x -> x)"
                            ~?= (EFix "f" (ELam "x" (EVar "x")))
    , "Parser If"       ~: parseExpr "if True then 1 else 2"
                            ~?= (EIf (ELit (LBool True)) (ELit (LInt 1)) (ELit (LInt 2)))
        ]

precedence =[
      "Parser Prec 01" ~: parseExpr "1 + 2 * 3"
                          ~?= (EOp Add
                               (ELit (LInt 1))
                               (EOp Mul (ELit (LInt 2))
                                        (ELit (LInt 3))))
    , "Parser Prec 02" ~: parseExpr "True == False || 1 + 2 > 1"
                           ~?= (EOp Or
                                (EOp Eq (ELit (LBool True))
                                 (ELit (LBool False)))
                                (EOp Gt (EOp Add (ELit (LInt 1))
                                         (ELit (LInt 2)))
                                 (ELit (LInt 1))))
    , "Parser Prec 03" ~: parseExpr "1 + f 2"
                           ~?= (EOp Add (ELit (LInt 1))
                                (EApp (EVar "f") (ELit (LInt 2))))
    , "Parser Prec 04" ~: parseExpr "let f = fun x -> x+1 in f 1"
                           ~?= (ELet "f"
                                (ELam "x" (EOp Add (EVar "x") (ELit (LInt 1))))
                                (EApp (EVar "f") (ELit (LInt 1))))
    , "Parser Prec 05" ~: parseExpr "(x)"
                           ~?= (EVar "x")
    , "Parser Prec 06" ~: parseExpr "f (1 + 2)"
                           ~?= (EApp (EVar "f")
                                (EOp Add (ELit (LInt 1)) (ELit (LInt 2))))
           ]

parserTests = expressions ++ precedence
