// Problems from chapter 9 of Smullyan's "To Mock a Mockingbird".

#{
*** PROBLEM 1 ***

* Note:

  The "composition condition" (i.e. that given [X] and [Y]
  there exists [Z] such that for all [x], [Z x = X (Y x)])
  is equivalent to the existence of the [B] combinator,
  since [Z = B X Y] implies [Z x = B X Y x = X (Y x)].
  We will use this in our computational solution.

* Problem:

  Given the combinators [B] and [M], for an arbitrary combinator [x],
  find [F], the fixpoint of [x], such that [x (F x) = F x]
  (Smullyan says [F] where we say [F x];
  it's easier to work with [F] as a function.)

* Non-computational solution:

  Given x and M, compose x and M to get A:

    Define [A = compose(x, M)]
    --> A y = x (M y)
    Define [F = M A]
    --> F = M A = A A = x (M A) = x F
    --> F = x F
    Therefore [F] is the fixpoint of [x]. QED.

* Computational solution:
};

// Note: B and M are built-in.
def F x = M (B x M);
// #p F x;  // TODO

F x;
#c;
#s;
#s;
#s;
#nl;
x (F x);
#c;
#s;
#{
  Therefore [F x = x (F x)].
  Therefore [F x] is the fixpoint of [x].
  QED.
};


#{
*** PROBLEM 2 ***

* Problem:

  Given conditions C1 and C2 (existence of [B] and [M]),
  there is at least one combinator [X] which is its own fixpoint i.e.

    exists X: X X = X

* Non-computational solution:

  Assuming conditions C1 and C2,
  by problem 1 all combinators have fixpoints.
  Therefore, the combinator [M] has a fixpoint.
  Therefore: 

    exists R: M R = R 

  But by definition of [M], [M R = R R]. 
  Therefore [R = R R]. QED.

* Computational solution:
};

#{  Compute the fixpoint of [M], called [F]:};
#nl;

def F = M (B M M);
#{def F = M (B M M);}; // TODO #p
#nl;
M F;
#c;
#s;
#nl;
F;
#c;
#s;
#s;
#s;
#{= M F};
#{= F F};

#{
  So [F F = F]. QED.
};


