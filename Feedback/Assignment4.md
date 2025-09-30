# Assignment 4 - General feedback

Great submissions this week 🙂

## 4.1

Many groups did not include running the examples in 4.1. Please include this. You can for example copy the output of the terminal and put it in a text file, or include a screenshot.

## 4.2

Many good solutions, however it was apparently a bit unclear how/where to write the functions. The idea was not to write the programs in F# but rather in our own mini language "Fun". If you include ParseAndRun (or Parse) you can use `fromString` written in `Parse.fs` with your program. You should see your programs as abstract syntax.
For instance the solution to 4.2.1 might be:

```fsharp
> fromString "let sum x = if x = 0 then 0 else x + sum (x - 1) in sum 1000 end";;
val it : Absyn.expr =
  Letfun
    ("sum", ["x"],
     If
       (Prim ("=", Var "x", CstI 0), CstI 0,
        Prim ("+", Var "x", Call (Var "sum", [Prim ("-", Var "x", CstI 1)]))),
     Call (Var "sum", [CstI 1000]))

> run it;;
val it : int = 500500
```

## 4.3

Very good, many correct solutions.

You can do something like this:

```fsharp
match fClosure with
| Closure (f, params, fBody, fDeclEnv) ->
  let values = List.map (fun arg -> Int(eval arg env)) arguments |> List.zip params
  let fBodyEnv = values @ (f, fClosure) :: fDeclEnv
  eval fBody fBodyEnv
| _ -> failwith "eval Call: not a function"
```

To create an environment where we can evaluate the body of a function, where each parameter has been bound to its evaluation.
You may notice that the `params` list needs to be of the same length as `arguments`, to prevent `List.zip` from breaking. This is completely fine because we are creating a first-order functional language. In a higher-order functional language, calling a function with fewer than the expected number of arguments should return a new function (partial application). But this is not a possibility in this language, so however it breaks is fine.

## 4.4/5 - Extend Lexer/Parser

It is **very** important that you add precedence to your language in your .fsy file. If you don't you will get **shift/reduce** errors when generating your parser. This essentially means that your language is ambiguous and it should be avoided at all cost.

For this exercise this means adding precedence to your OR and AND tokens, and you have to change AppExpr to no longer parse multiple arguments as multiple calls, but as a list of arguments.
Something like this for precedence:

```fsharp
%left ELSE              /* lowest precedence  */
%left OR                /* ADDED THIS LINE    */
%left AND               /* ADDED THIS LINE    */
%left EQ NE
%left GT LT GE LE
%left PLUS MINUS
%left TIMES DIV MOD
%nonassoc NOT           /* highest precedence  */
```

And this for taking arguments:

```fsharp
Arguments:
    AtExpr               { [$1]         }
    | AtExpr Arguments   { $1 :: $2     }

AppExpr:
    AtExpr Arguments     { Call($1, $2) }
;
```
