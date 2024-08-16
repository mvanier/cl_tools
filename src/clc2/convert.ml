open Ir2
module A = Ast

let convert_ski lambda =
  failwith "TODO"

let convert_skibc lambda =
  failwith "TODO"

let convert_bckw lambda =
  failwith "TODO"

let convert converter lambda =
  match converter with
    | A.SKI   -> convert_ski lambda
    | A.SKIBC -> convert_skibc lambda
    | A.BCKW  -> convert_bckw lambda
