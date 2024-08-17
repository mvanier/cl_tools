(* Coq proofs of problems in _To Mock a Mockingbird_, chapter 9. *)

(*** Problem 1 ***)

(* Informal proof: *)

(*

Given:

  - the M combinator
  - the composition condition C1

then for an arbitrary combinator [x],
find [F], the fixpoint of [x], such that [x F = F].

The composition condition is:

  forall A B, exists C, forall x, C x = A (B x).

Solution:

  Given x and M, compose x and M to get A:

    Define [A = compose(x, M)]
    --> A y = x (M y)
    Define [F = M A]
    --> F = M A = A A = x (M A) = x F
    --> F = x F
    Therefore [F] is the fixpoint of [x]. QED.

*)

