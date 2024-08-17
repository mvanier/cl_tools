let basis = "
def B f g x = f (g x);
def C f x y = f y x;
def I x = x;
def K x y = x;
def L f x = f (x x);
def M x = x x;
// def Q = K I;
def S f g x = f x (g x);
def W f x = f x x;
// def X x = x K S K;
// def X* x = x S K;
"
