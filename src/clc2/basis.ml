let basis = "
def I x = x;
def K x y = x;
def B f g x = f (g x);
def C f x y = f y x;
def W f x = f x x;
def S f g x = f x (g x);
def X f = x K S K;
"
