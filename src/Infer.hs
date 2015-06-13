{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- Type inference using algorithm M
-- which is a context-sensitive, top down algorithm.

module Infer (inferExpr, infer, subst) where

import Syntax
import Data.List(foldl', lookup, union, (\\), delete, nub)

-- Type substitutions
-- [τ1/α1,...,τn/αn]
type TypeSubst = [(TypeId, Type)]

------------------------------
-- Occur check
------------------------------
occursCheck :: TypeId -> Type -> Bool
occursCheck a (TBase t)    = False
occursCheck a (TVar b)     = a == b
occursCheck a (TFun t1 t2) = occursCheck a t1 || occursCheck a t2

------------------------------
-- Applying type substitution
------------------------------
class Substitutable t where
    subst    :: TypeSubst -> t -> t
    freevars :: t -> [TypeId]

instance Substitutable Type where
    subst s (TBase t)  = s `seq` TBase t
        -- An error may occur in `s`.
    subst [] t = t
    subst ((a,t):s) (TVar b)
        | a == b    = subst s t
        | otherwise = subst s (TVar b)
    subst s (TFun t1 t2) = TFun (subst s t1) (subst s t2)

    freevars TBase{}      = []
    freevars (TVar a)     = [a]
    freevars (TFun t1 t2) = freevars t1 `union` freevars t2

instance Substitutable TypeScheme where
    -- S σ = ∀β.S{β/a}t
    subst s (Forall as t) = Forall as $ subst s' t
        where
          s' = dropWhile (eq as) s
          eq [] _         = False
          eq (x:xs) (a,t) = x == a || eq xs (a,t)

    freevars (Forall as t) = freevars t \\ as

instance Substitutable TypeEnv where
    subst s tenv = [(v,subst s σ) | (v,σ) <- tenv]

    freevars tenv = foldr union [] [freevars(σ) | (_, σ) <- tenv]

-- Composition of type substitutions
compose :: TypeSubst -> TypeSubst -> TypeSubst
s2 `compose` s1 = s1' ++ s2
    where s1' =  map (\(a, t) -> (a, subst s2 t)) s1

------------------------------
-- Unification
------------------------------
unify :: Type -> Type -> TypeSubst
unify (TBase t1) (TBase t2) | t1 == t2 = []
unify (TFun t11 t12) (TFun t21 t22) = s1 `compose` s2
    where
      s1 = unify t11 t21
      s2 = unify (subst s1 t12) (subst s1 t22)
unify (TVar a) t  = bind a t
unify t (TVar a)  = bind a t
unify t1 t2 =
    error $ "unification error: Contradicting constraints  " ++
              "[" ++ show t1 ++ "∽" ++ show t2 ++ "]"

bind :: TypeId -> Type -> TypeSubst
bind a t
    | t == TVar a             = []
    | occursCheck a t == True = error $ "unification error: Infinite type"
    | otherwise               = [(a, t)]

------------------------------
-- Type inference
------------------------------
-- Generate a fresh type variable
fresh :: TypeId -> (Type, TypeId)
fresh n = (TVar n, n+1)

freshvars 0 n = ([], n)
freshvars i n = let (v, n1)  = fresh n
                    (vs, n2) = freshvars (i-1) n1
                in (v:vs, n2)

-- Type schemes of binary operators
scheme Add = Forall [] (TFun (TBase TInt) (TFun (TBase TInt) (TBase TInt)))
scheme Sub = Forall [] (TFun (TBase TInt) (TFun (TBase TInt) (TBase TInt)))
scheme Mul = Forall [] (TFun (TBase TInt) (TFun (TBase TInt) (TBase TInt)))

scheme And = Forall [] (TFun (TBase TBool) (TFun (TBase TBool) (TBase TBool)))
scheme Or  = Forall [] (TFun (TBase TBool) (TFun (TBase TBool) (TBase TBool)))

scheme Eq  = Forall [0] (TFun (TVar 0) (TFun (TVar 0) (TBase TBool)))
scheme Gt  = Forall [] (TFun (TBase TInt) (TFun (TBase TInt) (TBase TInt)))
scheme Lt  = Forall [] (TFun (TBase TInt) (TFun (TBase TInt) (TBase TInt)))

-- Generalize a type to a type scheme
generalize :: TypeEnv -> Type -> TypeScheme
generalize tenv t = Forall as t
    where as = (freevars t) \\ (freevars tenv)

-- Instantiate a type scheme to a type which has new type variables
-- without universal quantifiers.
--
-- instantiate(∀α1...αn.τ)
--  --->  [β1...βn/α1...αn]τ  (which may occur type variables β1...βn)
instantiate :: TypeScheme -> TypeId -> (Type, TypeId)
instantiate (Forall as t) n =
    let (as',n') = freshvars (length as) n
        s   = zip as as'
    in (subst s t, n')

-- Type inference
infer :: TypeEnv -> Expr -> Type -> TypeId -> (TypeSubst, TypeId)

infer tenv (ELit (LInt _)) t n  = (unify t (TBase TInt), n)
infer tenv (ELit (LBool _)) t n = (unify t (TBase TBool), n)

infer tenv (EOp op e1 e2) t n = (s3 `compose` s2 `compose` s1, n3)
    where (TFun t1 (TFun t2 t3),n1) = instantiate (scheme op) n
          s1      = unify t t3
          (s2,n2) = infer (subst s1 tenv) e1 (subst s1 t1) n1
          (s3,n3) = infer (subst (s2 `compose` s1) tenv) e2 (subst (s2 `compose` s1) t2) n2

infer tenv (EVar x) t n = (unify t t', n')
    where (t',n') = instantiate σ n
          σ       = lookupEnv x tenv
infer tenv (ELam x e) t n =
    let (b1, n1) = fresh n
        (b2, n2) = fresh n1
        s1       = unify t (TFun b1 b2)
        b1'      = subst s1 b1
        b2'      = subst s1 b2
        tenv'    = subst s1 tenv
        (s2,n3)  = infer (tenv' `ext` (x, Forall [] b1')) e b2' n2
    in (s2 `compose` s1, n3)

infer tenv (EApp e1 e2) ρ n =
    let (b,n1)  = fresh n
        (s1,n2) = infer tenv e1 (TFun b ρ) n1
        (s2,n3) = infer (subst s1 tenv) e2 (subst s1 b) n2
    in (s2 `compose` s1, n3)

infer tenv (ELet x e1 e2) ρ n =
    let (b,n1)   = fresh n
        (s1, n2) = infer tenv e1 b n1
        σ        = generalize (subst s1 tenv) (subst s1 b)
        tenv'    = subst s1 tenv
        ρ'       = subst s1 ρ
        (s2, n3) = infer (tenv' `ext` (x, σ)) e2 ρ' n2
    in (s2 `compose` s1, n3)

infer tenv (EFix f e) ρ n =
    infer (tenv `ext` (f, Forall [] ρ)) e ρ n

infer tenv (EIf e e1 e2) ρ n =
    let (s1, n1) = infer tenv e (TBase TBool) n
        ρ1       = subst s1 ρ
        tenv1    = subst s1 tenv
        (s2, n2) = infer tenv1 e1 ρ1 n1
        ρ2       = subst s2 ρ1
        tenv2    = subst s2 tenv1
        (s3, n3) = infer tenv2 e2 ρ2 n2
    in (s1 `compose` s2 `compose` s3, n3)


inferExpr :: Expr -> Type
inferExpr e = t'
    where
      (t, n) = fresh 0
      (s, _) = infer [] e t n
      t'     = (subst s t)
