{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- Type inference using algorithm M
-- which is a context-sensitive, top down algorithm.

module Infer (inferExpr, infer, subst) where

import Syntax
import Data.List

-- Type substitutions
-- [τ1/α1,...,τn/αn]
type TypeSubst = [(TypeId, Type)]

------------------------------
-- Free type variables
------------------------------
class Free t where
    ftv :: t -> [TypeId]

instance Free Type where
    ftv TBase{}  = []
    ftv (TVar a) = [a]
    ftv (TFun t1 t2) = ftv t1 `union` ftv t2

instance Free TypeScheme where
    ftv (Forall as t) = ftv t \\ as

instance Free TypeEnv where
    ftv tenv = foldr union [] [ftv(σ) | (_, σ) <- tenv]

------------------------------
-- Type substitution
------------------------------
-- Auxiliary functions
supp :: TypeSubst -> [TypeId]
supp s = [a | (a,_) <- s, subst s (TVar a) /= (TVar a)]

class Substitutable t where
    subst    :: TypeSubst -> t -> t

instance Substitutable Type where
    subst s (TBase t)  = s `seq` TBase t
        -- An error may occur in `s`.
    subst [] t = t
    subst ((a,t):s) (TVar b)
        | a == b    = subst s t
        | otherwise = subst s (TVar b)
    subst s (TFun t1 t2) = TFun (subst s t1) (subst s t2)

instance Substitutable TypeScheme where
    -- S σ = ∀α.S'τ  where S' = S \ [τ/α]
    subst s (Forall as t) = Forall as $ subst s' t
        where
          s' = dropWhile (eq as) s
          eq [] _         = False
          eq (x:xs) (a,t') = x == a || eq xs (a,t')

instance Substitutable TypeEnv where
    subst s tenv = [(v,subst s σ) | (v,σ) <- tenv]

-- Composition of type substitutions
compose :: TypeSubst -> TypeSubst -> TypeSubst
r `compose` s =
    [(a, subst r (subst s (TVar a))) | a <- supp(s)]
    `union` [(a, subst r (TVar a)) | a <- supp(r) \\ supp(s)]

------------------------------
-- Occur check
------------------------------
occursCheck :: TypeId -> Type -> Bool
occursCheck _ TBase{}    = False
occursCheck a (TVar b)     = a == b
occursCheck a (TFun t1 t2) = occursCheck a t1 || occursCheck a t2

------------------------------
-- Unification
------------------------------
unify :: Type -> Type -> TypeSubst
unify (TBase t1) (TBase t2) | t1 == t2 = []
unify (TFun t11 t12) (TFun t21 t22) = s1 `compose` s2
    where
      s1 = unify t11 t21
      s2 = unify (subst s1 t12) (subst s1 t22)
unify (TVar a) t | occursCheck a t == False = [(a,t)]
unify t (TVar a)  = unify (TVar a) t
unify t1 t2 =
    error $ "unification error: " ++
              "[" ++ show t1 ++ "∽" ++ show t2 ++ "]"

------------------------------
-- Type inference
------------------------------
-- Generate a fresh type variable
fresh :: TypeId -> (Type, TypeId)
fresh n = (TVar n, n+1)
-- Generate fresh type variables
freshvars :: Int -> TypeId -> ([Type], TypeId)
freshvars 0 n = ([], n)
freshvars i n = let (v, n1)  = fresh n
                    (vs, n2) = freshvars (i-1) n1
                in (v:vs, n2)

-- Type schemes of binary operators
scheme :: BinOp -> TypeScheme
scheme Add = Forall [] (TFun (TBase TInt) (TFun (TBase TInt) (TBase TInt)))
scheme Sub = Forall [] (TFun (TBase TInt) (TFun (TBase TInt) (TBase TInt)))
scheme Mul = Forall [] (TFun (TBase TInt) (TFun (TBase TInt) (TBase TInt)))

scheme And = Forall [] (TFun (TBase TBool) (TFun (TBase TBool) (TBase TBool)))
scheme Or  = Forall [] (TFun (TBase TBool) (TFun (TBase TBool) (TBase TBool)))

scheme Eq  = Forall [0] (TFun (TVar 0) (TFun (TVar 0) (TBase TBool)))
scheme Gt  = Forall [] (TFun (TBase TInt) (TFun (TBase TInt) (TBase TBool)))
scheme Lt  = Forall [] (TFun (TBase TInt) (TFun (TBase TInt) (TBase TBool)))

-- Generalize a type to a type scheme
generalize :: TypeEnv -> Type -> TypeScheme
generalize tenv t = Forall as t
    where as = (ftv t) \\ (ftv tenv)

-- Instantiate a type scheme to a type which has new type variables
-- without universal quantifiers.
--
-- instantiate(∀α1...αn.τ)
--  --->  [β1...βn/α1...αn]τ  (which may occur type variables β1...βn)
instantiate :: TypeScheme -> TypeId -> (Type, TypeId)
instantiate (Forall as t) n =
    let (bs,n') = freshvars (length as) n
        s   = zip as bs
    in (subst s t, n')

-- Type inference
infer :: TypeEnv -> Expr -> Type -> TypeId -> (TypeSubst, TypeId)

infer _ (ELit (LInt _))  ρ n = (unify ρ (TBase TInt), n)
infer _ (ELit (LBool _)) ρ n = (unify ρ (TBase TBool), n)

infer tenv (EOp op e1 e2) ρ n = (s3 `compose` s2 `compose` s1, n3)
    where (TFun t1 (TFun t2 t3),n1) = instantiate (scheme op) n
          s1      = unify ρ t3
          (s2,n2) = infer (subst s1 tenv) e1 (subst s1 t1) n1
          (s3,n3) = infer (subst (s2 `compose` s1) tenv) e2 (subst (s2 `compose` s1) t2) n2

infer tenv (EVar x) ρ n = (unify ρ ρ', n')
    where (ρ',n') = instantiate σ n
          σ       = lookupEnv x tenv

infer tenv (ELam x e) ρ n =
    let (b1, n1) = fresh n
        (b2, n2) = fresh n1
        s1       = unify ρ (TFun b1 b2)
        (s2,n3)  = infer ((subst s1 tenv) `ext` (x, Forall [] (subst s1 b1)))
                   e (subst s1 b2) n2
    in (s2 `compose` s1, n3)

infer tenv (EApp e1 e2) ρ n =
    let (b,n1)  = fresh n
        (s1,n2) = infer tenv e1 (TFun b ρ) n1
        (s2,n3) = infer (subst s1 tenv) e2 (subst s1 b) n2
    in (s2 `compose` s1, n3)

infer tenv (ELet x e1 e2) ρ n =
    let (b,n1)   = fresh n
        (s1,n2)  = infer tenv e1 b n1
        σ        = generalize (subst s1 tenv) (subst s1 b)
        (s2,n3)  = infer ((subst s1 tenv) `ext` (x, σ)) e2 (subst s1 ρ) n2
    in (s2 `compose` s1, n3)

infer tenv (EFix f e) ρ n =
    infer (tenv `ext` (f, Forall [] ρ)) e ρ n

infer tenv (EIf e1 e2 e3) ρ n =
    let (s1,n1)  = infer tenv e1 (TBase TBool) n
        (s2,n2)  = infer (subst s1 tenv) e2 (subst s1 ρ) n1
        (s3,n3)  = infer (subst (s2 `compose` s1) tenv) e3 (subst (s2 `compose` s1) ρ) n2
    in (s3 `compose` s2 `compose` s1, n3)


inferExpr :: Expr -> Type
inferExpr e = t
    where
      (ρ, n) = fresh 0
      (s, _) = infer [] e ρ n
      t      = (subst s ρ)
