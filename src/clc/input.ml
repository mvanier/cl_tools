type 'a stream = Cons of 'a * 'a stream Lazy.t
type 'a input = 'a stream ref

let make_input get_input =
  let rec iter () : 'a stream =
    Cons (get_input (), lazy (iter ()))
  in
    ref (iter ())

let next input =
  let Cons (h, lt) = !input in
    (input := Lazy.force lt; h)

let peek input = let Cons (h, _) = !input in h
let skip input = ignore (next input)

let get_stream input = !input

let rewind input stream =
  input := stream

