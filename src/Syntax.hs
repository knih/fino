module Syntax where

----------------------------------------
-- Expressions
----------------------------------------
type Id  = String
type Env = [(Id,Value)]

data Expr = ELit Lit
          | EOp BinOp Expr Expr  -- e1 * e2
          | EVar Id              -- x
          | ELam Id Expr         -- \x -> e
          | EApp Expr Expr       -- e1 e2
          | ELet Id Expr Expr    -- let x = e1 in e2
          | EIf Expr Expr Expr   -- if e1 then e2 else e3
          | EFix Id Expr         -- fix f \x.e
            deriving (Show, Eq, Ord)

data Lit = LInt Int
         | LBool Bool
           deriving (Show, Eq, Ord)

data BinOp = Add | Sub | Mul
           | And | Or
           | Eq | Gt | Lt
             deriving (Show, Eq, Ord)

data Value = VInt  Int
           | VBool Bool
           | VClos Id Expr Env
             deriving (Show, Ord, Eq)

----------------------------------------
-- Types
----------------------------------------
type TypeId = Int

data Base = TInt
          | TBool
            deriving (Show, Eq, Ord)

data Type = TBase Base         -- base types
          | TFun Type Type     -- function types
          | TVar TypeId        -- type variables
            deriving (Show, Eq, Ord)

-- A type scheme ∀a1...an.τ is a type that may contain occurrences of type
-- variables with universal quantifiers.
-- 
-- e.g) A type scheme ∀αβ.α → β → α is represented as
--      `Forall [0,1] TFun (TVar 0) (TFun (TVar 1) (TVar 0))`.
data TypeScheme = Forall [TypeId] Type
                deriving (Show, Eq, Ord)

type TypeEnv = [(Id, TypeScheme)]
