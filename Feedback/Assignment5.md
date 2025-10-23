# Assignment 5 - General feedback

## 5.1 - Merge sort
Many great solutions, this assignment mostly just shows how compact the functional solution can be.

## 5.7 - List type

This assignment was much easier if you started in the TypedFun.fs file in TypedFun, not TypeInference.fs in the Fun project. 
The goal was to make the `typ` function work for lists, so we can typecheck these 
```
let shouldSucceed = typeCheck (ListExpr([CstI 1; CstI 1], TypI));;
let shouldFail = typeCheck (ListExpr([CstI 1; CstB false], TypI));;
```
This can be done by extending the typ function to match ListExpr, and recursively calling typ on each element of the list, and checking that they match the list type.


## 6.2 and 6.3 - Add anonymous functions

Anonymous functions can be used like so:

```fsharp
// Calls an anonymous function which multiplies an argument with 2, with an argument 10
// The left hand side is parsed as a closure. The whole thing is parsed as a "Call"
// to the closure with the value 10.
(fun x -> x * 2) 10

// f is a higher-order function that applies 10 to a given function g
let f g = g 10 in f (fun x -> x * 2) end
```

To implement them we add the new anonymous function Absyn.fs, and the type Clos in HigherFun.fs, which is a Closure without a name. 
To handle this closure in eval, we do the same as with a regular closure, the only difference being that we don’t add a function name to the environment (since it has none). 
Notice that this is because anonymous functions cannot be called recursively.

```fsharp
    | Call(eFun, eArg) ->
      let fClosure = eval eFun env  (* Different from Fun.fs - to enable first class functions *) // NAME arg -> Call($1, $2) -> f 2
      match fClosure with
      | Closure (f, x, fBody, fDeclEnv) ->
        let xVal = eval eArg env
        let fBodyEnv = (x, xVal) :: (f, fClosure) :: fDeclEnv
        in eval fBody fBodyEnv
      | Clos (x, fBody, fDeclEnv) ->
        let xVal = eval eArg env
        let fBodyEnv = (x, xVal) :: fDeclEnv
        in eval fBody fBodyEnv
      | _ -> failwith "eval Call: not a function"
    | Fun (x, fBody) -> Clos (x, fBody, env)
```

## 6.3 - Parsing anonymous functions

Great solutions for this. 

Remember to check that you do not get shift/reduce errors in your fsy file. 
You most likely get shift/reduce errors if you forgot to specify precedence on the arrow "->" token, or if added anonymous functions to the `AtExpr` production, instead of the `Expr` production.


```fsharp
Expr:
    AtExpr                              { $1                     }
  | AppExpr                             { $1                     }
  | FUN NAME RARROW Expr                { Fun($2, $4)            } // Added this line
  | IF Expr THEN Expr ELSE Expr         { If($2, $4, $6)         }
  ...
;
```

 
## 6.4 - Type trees

Many great type trees. One common problem is using p1, to end branches that are not integers. Remember that for variables and functions we use p3 to look them up in the environment.

If you have not done so yet, it is also highly recommend that you read through the PDF "Tutorial on Polymorphic Type Derivations" found on learnit.


## 6.5.2 - Write programs of specific types

The main take away is getting an understanding of how the type inference somewhat magically can tell us the types of functions, even if the functions don’t really make sense to use.

If you were wondering about the last 2 programs, here is an example:
- 'a -> 'b: `"let f x = f x in f end"`
- 'a: `"let f x = f x in f 1 end"`
These programs are of course functionally nonsensical, as they result in infinite recursion, but they are still typeable.


