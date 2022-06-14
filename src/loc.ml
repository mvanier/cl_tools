open Lexing

type loc = 
  { source_name : string;
    start_line  : int;
    start_char  : int;
    end_line    : int;
    end_char    : int }

let dummy_loc =
  { source_name = "";
    start_line  = 0;
    start_char  = 0;
    end_line    = 0;
    end_char    = 0 }

let string_of_loc l = 
  (if l.source_name = "" 
   then "<interactive>; " 
   else "source: " ^ l.source_name ^ "; ") ^
  Printf.sprintf "line %d, char %d to line %d, char %d" 
    l.start_line l.start_char l.end_line l.end_char

let string_of_loc_short l = 
  let get_loc_string sl sc el ec = 
    match () with
      | _ when sl = el && sc = ec -> Printf.sprintf "%d:%d" sl sc
      | _ when sl = el -> Printf.sprintf "%d:%d-%d" sl sc ec
      | _ -> Printf.sprintf "%d:%d-%d:%d" sl sc el ec
  in
    (l.source_name ^ ": ") ^ 
    (get_loc_string l.start_line l.start_char l.end_line l.end_char)

let get_loc fn p s = 
  let ln = p.pos_lnum in
  let sc = p.pos_cnum - p.pos_bol in
  let loc = { source_name = fn;
              start_line  = ln;
              end_line    = ln;
              start_char  = sc + 1;
              end_char    = sc + String.length s }
  in loc

let span loc1 loc2 = 
  match () with
    | _ when loc1.source_name <> loc2.source_name ->
      failwith "invalid location span (different source_names)"
    | _ when loc1.start_line > loc2.start_line ->
      failwith "invalid location span (loc1 comes after loc2)"
    | _ when loc1.start_line = loc2.start_line &&
             loc1.start_char > loc2.start_char ->
      failwith "invalid location span (loc1 comes after loc2)"
    | _ -> { source_name   = loc1.source_name;
             start_line = loc1.start_line;
             start_char = loc1.start_char;
             end_line   = loc2.end_line;
             end_char   = loc2.end_char }

let loc_eq loc1 loc2 =
  loc1.source_name = loc2.source_name &&
  loc1.start_line  = loc2.start_line &&
  loc1.start_char  = loc2.start_char &&
  loc1.end_line    = loc2.end_line &&
  loc1.end_char    = loc2.end_char

