#{
Computing the normal-order Y combinator in various combinator bases.

The normal order Y combinator has this form:

  Y = \f . (\x . x x) (\x . f (x x))

Let's convert it using various bases.
};

#{1. SKI combinators only:

#convert :ski \f . (\x . x x) (\x . f (x x))
};
#convert :ski \f . (\x . (x x)) (\x . (f (x x)));

