(* Combinator calculator. *)

open Ccllib
open Types
open Eval

let prim p = Atom (Prim p)

let s = prim S
let k = prim K
let i = prim I
let b = prim B
let c = prim C
let w = prim W

let var i = Atom (Var i)

let x = var "x"
let y = var "y"
let z = var "z"
let v1 = var "v1"
let v2 = var "v2"
let _v3 = var "v3"

let comb i = Atom (Comb i)

(* Add some derived definitions. *)
let _ = add_to_env "M" (List [w; i])
let _ = add_to_env "A" (List [k; i])
let _ = add_to_env "T" (List [c; i])

(* Tests. *)
(* Definitions of I: *)
let _e1 = List [s; k; k; x]
let _e2 = List [w; k; x]
(* Definition of C from S, K, and B: *)
let _e3 = List [s; List [b; b; s]; List [k; k]; x; y; z]
(* C1 = B C s.t. C1 x y z w = x y w z: *)
let _e4 = List [b; c; x; y; z; v1]
(* C2 = B C1 s.t. C2 x y z w a = x y z a w: *)
let _e5 = List [b; List [b; c]; x; y; z; v1; v2]
let e6 = List [comb "M"; x]


let _ = 
  let e = e6 in
  let de = desugar e in
    begin
      Printf.printf "%s\n" (string_of_expr e);
      Printf.printf "--> %s\n" (string_of_expr2 de);
      Printf.printf "--> %s\n" (string_of_expr2 (eval_expr2 de))
    end

