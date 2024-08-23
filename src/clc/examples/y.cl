#{
Computing the normal-order Y combinator in various combinator bases.

The normal order Y combinator has this form:

  Y = \f . (\x . x x) (\x . f (x x))

Let's convert it using various bases.
};

#{
1. SKI combinators:

#convert :ski \f . (\x . (x x)) (\x . f (x x))
};
#convert :ski \f . (\x . (x x)) (\x . (f (x x)));
#nl;

#{
2. SKIBC combinators:

#convert :skibc \f . (\x . (x x)) (\x . f (x x))
};
#convert :skibc \f . (\x . (x x)) (\x . (f (x x)));
#nl;

#{
3. BCKW combinators:

#convert :bckw \f . (\x . (x x)) (\x . f (x x))
};
#convert :bckw \f . (\x . (x x)) (\x . (f (x x)));
#nl;

#{
4. BCKWI combinators:

#convert :bckwi \f . (\x . (x x)) (\x . f (x x))
};
#convert :bckwi \f . (\x . (x x)) (\x . (f (x x)));
#nl;

