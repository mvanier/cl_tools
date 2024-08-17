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

#{----};
#p B f g x;
#p M x;
def F x = M (B x M);
#p F x;
#nl;

F x;
#c;
#sn 3;
#nl;
x (F x);
#c;
#s;
#{
// Therefore [F x = x (F x)].
// Therefore [F x] is the fixpoint of [x].
// QED.
----
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

----
// Compute the fixpoint of [M], called [F]:
};

#display-raw;
def F = M (B M M);
#p F;
#nl;

F F;
#c;
#sn 2;
#{// Reduce F:};
#sl :1;

#nl;

F;
#c;
#sn 5;
#display-normal;

#{
// So [F F = F]. QED.
----
};


#{
*** PROBLEM 3 ***

* Problem:

  Given the B combinator and a combinator A with the property:

    forall X . exists x . (A x) = (X x)

  (but no M combinator), then show that:

    forall X . exists y . (X y) = y

  i.e. that all birds are fond of at least one bird.

* Non-computational solution:

  Pick a combinator [X].
  Find [y] such that [X y = y].

  Consider [Z = B X A] (composition of X and A).
  By the definition of [A], we have:

    exists r . A r = Z r
    Set r = r'
    --> A r' = Z r'
    --> A r' = B X A r' = X (A r')

  Let [y = A r'].
  Then [y = X y].  QED.

* Bonus question:

  Is it true that

    forall X . exists x . M x = X x

  ?

  Answer:

    M x = x x

  so [x = X] and the answer is yes.

  So if [M] exists, then [M] is the [A] combinator required for this problem.
  Therefore, problem 1 is a special case of problem 3 with [A = M].
  A Mockingbird is agreeable!

* Computational solution:

----
};

#{** Part 1.};
#nl;

def Z = B X A;
#{By definition of [A], [exists r . A r = Z r].
Pick [r = r'].
So [A r' = Z r'].
};
Z r';
#c;
#sn 2;
#{
So [A r' = X (A r')], so [X] is fond of at least one bird ([A r']). QED.
};

#{** Part 2.};
#nl;

#{For arbitrary X:};
#nl;

M X;
#n;

#{
so the question

  For arbitrary X, is [exists x . M x = X x] true?

is trivially satisfied with [x = X].  QED.

----
};

