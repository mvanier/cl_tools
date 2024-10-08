open Ir2
module A = Ast

let rec occurs_free id lexpr =
  match lexpr with
    | A.LVar id' when id = id' -> true
    | A.LApp (l1, l2) ->
        occurs_free id l1 || occurs_free id l2
    | A.LLam (id', l) ->
        if id = id' then
          false
        else
          occurs_free id l
    | _ -> false

let rec lambda_of_expr = function
  | Var id ->
      A.LVar id
  | Const id ->
      A.LConst id
  | App (e1, e2) ->
      A.LApp (lambda_of_expr e1, lambda_of_expr e2)

let rec convert_ski lexpr =
  match lexpr with
    | A.LVar id ->
        Var id

    | A.LConst id ->
        Const id

    | A.LApp (l1, l2) ->
        App (convert_ski l1, convert_ski l2)

    | A.LLam (x, LVar x') when x = x' ->
        Const "I"

    | A.LLam (x, A.LApp (l, A.LVar x'))
        when x = x' && not (occurs_free x l) ->
        (* eta reduction rule *)
        convert_ski l

    | A.LLam (x, l) when not (occurs_free x l) ->
        App (Const "K", convert_ski l)

    | A.LLam (x, A.LLam (y, l)) when occurs_free x l ->
        let inner = convert_ski (A.LLam (y, l)) in
        let inner' = lambda_of_expr inner in
          convert_ski (A.LLam (x, inner'))

    | A.LLam (x, A.LApp (l1, l2)) ->
        let inner1 = convert_ski (A.LLam (x, l1)) in
        let inner2 = convert_ski (A.LLam (x, l2)) in
          App (App (Const "S", inner1), inner2)

    (* The above cases are exhaustive,
       but it's hard to convince the compiler of this. *)
    | _ -> failwith "convert_ski: this should not happen"

let rec convert_skibc lexpr =
  match lexpr with
    | A.LVar id ->
        Var id

    | A.LConst id ->
        Const id

    | A.LApp (l1, l2) ->
        App (convert_skibc l1, convert_skibc l2)

    | A.LLam (x, LVar x') when x = x' ->
        Const "I"

    | A.LLam (x, A.LApp (l, A.LVar x'))
        when x = x' && not (occurs_free x l) ->
        (* eta reduction rule *)
        convert_skibc l

    | A.LLam (x, l) when not (occurs_free x l) ->
        App (Const "K", convert_skibc l)

    | A.LLam (x, A.LLam (y, l)) when occurs_free x l ->
        let inner = convert_skibc (A.LLam (y, l)) in
        let inner' = lambda_of_expr inner in
          convert_skibc (A.LLam (x, inner'))

    | A.LLam (x, A.LApp (l1, l2)) 
        when occurs_free x l1 && not (occurs_free x l2) ->
        let inner1 = convert_skibc (A.LLam (x, l1)) in
        let inner2 = convert_skibc l2 in
          App (App (Const "C", inner1), inner2)

    | A.LLam (x, A.LApp (l1, l2))
        when not (occurs_free x l1) && occurs_free x l2 ->
        let inner1 = convert_skibc l1 in
        let inner2 = convert_skibc (A.LLam (x, l2)) in
          App (App (Const "B", inner1), inner2)

    | A.LLam (x, A.LApp (l1, l2)) ->
        let inner1 = convert_skibc (A.LLam (x, l1)) in
        let inner2 = convert_skibc (A.LLam (x, l2)) in
          App (App (Const "S", inner1), inner2)

    (* The above cases are exhaustive,
       but it's hard to convince the compiler of this. *)
    | _ -> failwith "convert_skibc: this should not happen"

(* NOTE:
   The BCKW and BCKWI conversions are not very good.
   They often produce much larger than optimal expressions. *)

let rec convert_bckw lexpr =
  match lexpr with
    | A.LVar id ->
        Var id

    | A.LConst id ->
        Const id

    | A.LApp (l1, l2) ->
        App (convert_bckw l1, convert_bckw l2)

    | A.LLam (x, LVar x') when x = x' ->
        App (Const "W", Const "K")

    | A.LLam (x, A.LApp (l, A.LVar x'))
        when x = x' && not (occurs_free x l) ->
        (* eta reduction rule *)
        convert_bckw l

    | A.LLam (x, l) when not (occurs_free x l) ->
        App (Const "K", convert_bckw l)

    | A.LLam (x, A.LLam (y, l)) when occurs_free x l ->
        let inner = convert_bckw (A.LLam (y, l)) in
        let inner' = lambda_of_expr inner in
          convert_bckw (A.LLam (x, inner'))

    | A.LLam (x, A.LApp (l1, l2)) 
        when occurs_free x l1 && not (occurs_free x l2) ->
        let inner1 = convert_bckw (A.LLam (x, l1)) in
        let inner2 = convert_bckw l2 in
          App (App (Const "C", inner1), inner2)

    | A.LLam (x, A.LApp (l1, l2))
        when not (occurs_free x l1) && occurs_free x l2 ->
        let inner1 = convert_bckw l1 in
        let inner2 = convert_bckw (A.LLam (x, l2)) in
          App (App (Const "B", inner1), inner2)

    | A.LLam (x, A.LApp (l1, l2)) ->
        let inner1 = convert_bckw (A.LLam (x, l1)) in
        let inner2 = convert_bckw (A.LLam (x, l2)) in
          App (Const "W",
               App (App (Const "B", 
                         App (Const "C", inner1)),
                    inner2))

    (* The above cases are exhaustive,
       but it's hard to convince the compiler of this. *)
    | _ -> failwith "convert_bckw: this should not happen"

let rec convert_bckwi lexpr =
  match lexpr with
    | A.LVar id ->
        Var id

    | A.LConst id ->
        Const id

    | A.LApp (l1, l2) ->
        App (convert_bckwi l1, convert_bckwi l2)

    | A.LLam (x, LVar x') when x = x' ->
        Const "I"

    | A.LLam (x, A.LApp (l, A.LVar x'))
        when x = x' && not (occurs_free x l) ->
        (* eta reduction rule *)
        convert_bckwi l

    | A.LLam (x, l) when not (occurs_free x l) ->
        App (Const "K", convert_bckwi l)

    | A.LLam (x, A.LLam (y, l)) when occurs_free x l ->
        let inner = convert_bckwi (A.LLam (y, l)) in
        let inner' = lambda_of_expr inner in
          convert_bckwi (A.LLam (x, inner'))

    | A.LLam (x, A.LApp (l1, l2)) 
        when occurs_free x l1 && not (occurs_free x l2) ->
        let inner1 = convert_bckwi (A.LLam (x, l1)) in
        let inner2 = convert_bckwi l2 in
          App (App (Const "C", inner1), inner2)

    | A.LLam (x, A.LApp (l1, l2))
        when not (occurs_free x l1) && occurs_free x l2 ->
        let inner1 = convert_bckwi l1 in
        let inner2 = convert_bckwi (A.LLam (x, l2)) in
          App (App (Const "B", inner1), inner2)

    | A.LLam (x, A.LApp (l1, l2)) ->
        let inner1 = convert_bckwi (A.LLam (x, l1)) in
        let inner2 = convert_bckwi (A.LLam (x, l2)) in
          App (Const "W",
               App (App (Const "B", 
                         App (Const "C", inner1)),
                    inner2))

    (* The above cases are exhaustive,
       but it's hard to convince the compiler of this. *)
    | _ -> failwith "convert_bckwi: this should not happen"

let convert converter lexpr =
  match converter with
    | A.SKI   -> convert_ski lexpr
    | A.SKIBC -> convert_skibc lexpr
    | A.BCKW  -> convert_bckw lexpr
    | A.BCKWI -> convert_bckwi lexpr
