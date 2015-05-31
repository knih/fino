# Fino: A Toy Functional Language with Polymorphic Type Inference
Fino is a tiny programming language which is an simple implementation of the algorithm M [^1] for Hindley/Milner style type systems in Haskell.

The algorithm M is a top-down (or context-sensitive) algorithm. M can always find type errors earlier than the algorithm W which is buttom-up.

I hope this help to implement let-polymorphic type inference.

## Syntax
The Following gives types and expressions in Fino.

Syntax:

```
e ::= c                 (constant literal)
    | e op e            (binary operator)
    | fun x -> e        (lambda abstraction)
    | e e               (application)
    | let e = e in e    (let-binding)
    | fix f e           (fixed-point)
    | if e then e else e
```

Types:

```
t ::= Int | Bool        (base type)
    | t -> t            (function type)
    | ∀a1...an.t        (type schema)
```


Also see `Syntax.hs`.


## Usage
You can use the `fino` command to open a Fino REPL.

```zsh
$ ./fino
Fino 0.1.0.0
>>> 
```
When you type a expression (and hit enter), Fino will then evaluate that expression.

```ocaml
>>> 1 + 2
expr:  EOp Add (ELit (LInt 1)) (ELit (LInt 2))
type:  TBase TInt
value: VInt 3 :: TBase TInt
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
value: VInt 2 :: TBase TInt
```

## References
[^1]: Oukseh Lee and Kwangkeun Yi. 1998. Proofs about a folklore let-polymorphic type inference algorithm. ACM Trans. Program. Lang. Syst. 20, 4 (July 1998), 707-723.
