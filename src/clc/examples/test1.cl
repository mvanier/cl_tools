// Test #1.

#{
This shows how to compute the B combinator using just S and K.
};

def B' = S (K S) K;
B' f g x;
#n ;

#{
This shows how to compute the C combinator using just S and K.
};

def C' = S (S (K (S (K S) K)) S) (K K);
C' f x y ;
#n ;

