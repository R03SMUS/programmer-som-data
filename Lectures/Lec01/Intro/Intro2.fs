(* Programming language concepts for software developers, 2010-08-28 *)

(* Evaluating simple expressions with variables *)

module Intro2

(* Association lists map object language variables to their values *)

let env = [("a", 3); ("c", 78); ("baf", 666); ("b", 111)];;

let emptyenv = [];; (* the empty environment *)

let rec lookup env x =
    match env with
    | []        -> failwith (x + " not found")
    | (y, v)::r -> if x=y then v else lookup r x;;

let cvalue = lookup env "c";;


(* Object language expressions with variables *)

type expr =
  | CstI of int
  | Var of string
  | Prim of string * expr * expr
  | If of expr * expr * expr

type aexpr = 
  | CstI of int
  | Var of string 
  | Add of aexpr * aexpr
  | Mul of aexpr * aexpr
  | Sub of aexpr * aexpr

let opgave1 = Sub(Var "v", Add(Var "w", Var "z"))
let opgave2 = Mul(CstI 2, Sub(Var "v", Add(Var "w", Var "z")))
let opgave3 = Add(Var "x", Add(Var "y", Add(Var "z", Var "v")))



let rec fmt input = 
    match input with
    | CstI i -> string i
    | Var x -> x
    | Add (a1, a2) -> $"({fmt a1} + {fmt a2})" 
    | Sub (a1, a2) -> $"({fmt a1} - {fmt a2})" 
    | Mul (a1, a2) -> $"({fmt a1} * {fmt a2})" 

let rec simplify input = 
    match input with
    | CstI _ -> input
    | Var _ -> input
    | Add(CstI 0, a) | Add(a, CstI 0) -> simplify a
    | Sub(a, CstI 0) -> simplify a
    | Mul(CstI 1, a) | Mul (a, CstI 1) -> simplify a
    | Mul(CstI 0, a) | Mul (a, CstI 0) -> CstI 0
    | Sub(a,b) when a = b -> CstI 0
    | Add(a,b) -> Add(simplify a, simplify b)
    | Sub(a,b) -> Sub(simplify a, simplify b)
    | Mul(a,b) -> Mul(simplify a, simplify b)
let fejl = Add(Add(CstI 10, CstI 0), CstI 100)

let rec diff apexr1 aexpr2 =
    match aexpr1, apexr2 with
    | _, CstI a -> CstI 0
    | Var x, Var x -> CstI 1 //or should this be CstI? instead of Var
    | Var x, Var y -> CstI 0 //or should this be CstI?
    | _, Add(e1, e2) ->  Add(diff apexr1 e1, diff apexr1 e2)
    | _, Sub(e1, e2) ->  Sub(diff apexr1 e1, diff apexr1 e2)
    | _, Mul(e1, e2) ->  Add(Mul(diff apexr1 e1, e2), Mul(diff apexr1 e2, e1))

let x = fmt opgave2

let e1 = CstI 17;;

let e2 = Prim("+", CstI 3, Var "a");;

let e3 = Prim("+", Prim("*", Var "b", CstI 9), Var "a");;

    (* max min == *)
let max1 = Prim("max", CstI 100, CstI 11)
let max2 = Prim("max", CstI 100, CstI 1001)
let min = Prim("min", CstI 20, CstI 11)
let min = Prim("min", CstI 20, CstI 110)
let equals1 = Prim("==", CstI 11, CstI 11)
let equals2 = Prim("==", CstI 101, CstI 11)
    (* max min == *)

let ifeval = If(Prim("==", CstI 10, CstI 12), CstI 123, CstI 321)

(* Evaluation within an environment *)

let rec eval e (env : (string * int) list) : int =
    match e with
    | CstI i            -> i
    | Var x             -> lookup env x
    | Prim(opt, e1, e2) -> 
        let i1 = eval e1 env
        let i2 = eval e2 env
        match opt with
        | "+" -> i1 + i2
        | "-" -> i1 - i2
        | "*" -> i1 * i2
        | "max" -> if i1 > i2  then i1 else i2
        | "min" -> if i1 < i2 then i1 else i2
        | "==" -> if i1 = i2 then 1 else 0 
        | _ -> failwith "unknown primitive bad! input!"
    | If(e1, e2, e3) -> if eval e1 env > 0 then eval e2 env else eval e3 env

let e1v  = eval e1 env;;
let e2v1 = eval e2 env;;
let e2v2 = eval e2 [("a", 314)];;
let e3v  = eval e3 env;;
let e4v = eval e4 env;;
