# Fino: A Toy Functional Language with Polymorphic Type Inference
Fino is a tiny programming language implemented in Haskell. 
It has a Hindley/Milner style polymorphic type system which is an simple implementation of the algorithm M [1].

The algorithm M is a top-down (or context-sensitive) algorithm. M can always find type errors earlier than the algorithm W which is buttom-up.

I hope this help to implement let-polymorphic type inference.

## Syntax
The Following gives types and expressions in Fino.

Syntax:

```
e ::= c                 (constant literal)
    | x                 (variable)
    | e op e            (binary operator)
    | fun x -> e        (lambda abstraction)
    | e e               (application)
    | let e = e in e    (let-binding)
    | let rec e = e in e
    | fix f e           (fixed-point)
    | if e then e else e
```

Types:

```
t ::= Int | Bool        (base type)
    | t -> t            (function type)
    | ∀a1...an.t       (type schema)
```

Also see `Syntax.hs`.


## Usage
You can use the `fino` command to open a Fino REPL.

```zsh
$ ./fino
Fino 0.2.1.0
>>> 
```
When you type a expression (and hit enter), Fino will then evaluate that expression.

```ocaml
>>> 1 + 2
expr:  EOp Add (ELit (LInt 1)) (ELit (LInt 2))
type:  TBase TInt
value: VInt 3
```

Type `:quit` to exit the REPL.

```ocaml
>>> :quit
```

## Examples

`==` is a polymorphic operator:

```ocaml
let n = 1 in
let b = n == 1 in
b == True
```

The identity function as a polymorphic function:

```ocaml
let id = (fun x -> x) in 
let n = id 1 in 
let b = id True 
in n + 1

expr:  ELet "id" (ELam "x" (EVar "x")) (ELet "n" (EApp (EVar "id") (ELit (LInt 1))) (ELet "b" (EApp (EVar "id") (ELit (LBool True))) (EOp Add (EVar "n") (ELit (LInt 1)))))
type:  TBase TInt
value: VInt 1
```

The Fibonatti function:
```ocaml
let fib = fix f (fun x ->
    if x==0 || x==1
    then 1
    else f (x-1) + f (x-2))
in fib 10

expr:  ELet "fib" (EFix "f" (ELam "x" (EIf (EOp Or (EOp Eq (EVar "x") (ELit (LInt 0))) (EOp Eq (EVar "x") (ELit (LInt 1)))) (ELit (LInt 1)) (EOp Add (EApp (EVar "f") (EOp Sub (EVar "x") (ELit (LInt 1)))) (EApp (EVar "f") (EOp Sub (EVar "x") (ELit (LInt 2)))))))) (EApp (EVar "fib") (ELit (LInt 10)))
type:  TBase TInt
value: VInt 89
```

The above expression can be written using `let rec` as follows:
```ocaml
let rec fib x =
    if x==0||x==1
    then 1
    else fib (x-1) + fib (x-2)
in fib 10
```

## References
[1]: Oukseh Lee and Kwangkeun Yi. 1998. Proofs about a folklore let-polymorphic type inference algorithm. ACM Trans. Program. Lang. Syst. 20, 4 (July 1998), 707-723.
