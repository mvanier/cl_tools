// Test #1.

#{
This shows how to compute the C combinator using just S and K.
};

def C' = S (S (K (S (K S) K)) S) (K K);
C' f x y ;
#n ;

