# Assignment 1 - General feedback

In general, great solutions from everyone. 🎉
It would be really nice if you made sure to verify that the solutions can run, either by loading the entire file ([#load “<filepath>”] and then [open module]) or by copying the code into F# interactive. Also really try to focus on readability, less is often more!
Try to avoid uploading build files. Usually these are in folders called, obj, bin, etc.

## 1.1

Some people had if statements implemented to evaluate both sides of the if, and then return the result from the chosen side, computing the side not chosen is unnecessary we can instead evaluate directly inside the if-statement:
` if eval e1 env <> 0 then eval e2 env else eval e3 env`

## 1.2

For fmt, you need () around each expr, otherwise you cant see the correct order, for example (Mul(Add(CstI 10, CstI 1), CstI 100)) becomes 10-1*100, but should be (10-1)*100

Some of you chose not to make the simplify function recursive, if you wanted it recursive you could do something like the following, where we match the result of the recursive call:

```fsharp
let rec simplify (e : aexpr) : aexpr =
    match e with
    | Add(x, y) ->
        match simplify x, simplify y with
        | CstI 0, z -> z
        | z, CstI 0 -> z
        | CstI a, CstI b -> CstI(a + b)
        | a, b -> Add(a, b)
     …
```

For the diff function, the most tricky part might be the mul, and making it recursive:

```fsharp
symbdiff : (e : aexpr) -> (s : string) -> aexpr

let rec symbdiff expr (x : string) =
    match expr with
    | CstI i -> CstI 0
    | Var v when v = x -> CstI 1
    | Var v  -> CstI 0
    | Add (a, b) -> Add (symbdiff a x, symbdiff b x)
    | Sub (a, b) -> Sub (symbdiff a x, symbdiff b x)
    | Mul (a, b) ->
      Add(Mul(symbdiff a x, b), Mul(a, symbdiff b x))
```

## 1.4

Many great solutions, the main point of this was to show the advantage of functional languages and abstract data types, compared to object-oriented languages.

## 2.1, 2.2, 2.3

The main difference in solutions here were whether you used a fold or recursion, but both are totally valid. Fold example:

```fsharp
let rec eval e (env : (string * int) list) : int =
    match e with
    ...
    | Let (erhs, ebody) ->
      let newEnv =
        List.fold (fun env' (v, e) -> (v, eval e env') :: env') env erhs
      eval ebody newEnv
    ...
```

Recursive example:

```fsharp
let rec eval e (env : (string * int) list) : int =
    match e with
    ...
    | Let ([], ebody) -> eval ebody env
    | Let ((s, expr) :: xs, ebody) ->
      let newEnv = (s, eval expr env) :: env
      eval (Let(xs, ebody)) newEnv
    ...

```
