import Types
import Parser
import Pretty
import Data.Maybe

{-
Current plans:

Either find a way to map variable names or (for now) simply make var a (Int, String) and set its instance for
Eq to equating ONLY the indecies. Then "show" can just use the string associated with every variable.
Since lams don't need indecies in de brujin notation, they can just store a string which will correspond to the same string of the
variables that it binds.

With that out of the way, change the parser to accomodate the new definition of var and instantiate vars with a default,
meaningless index value.

Once the whole expression has been parsed, you can feed it to another function which recurses down the structure with a context:
[(Name, Int)] and stores the names of lams it sees and increments every int in the tuples every time it goes down another lam.

This shouldn't be too hard except for the fact that there is no way to deal with free variables. Thinking about it, I don't know
if I necessarily need to deal with free variables since they effectively correspond to undefined variables which will end up
failing at the type checking stage anyway. If I did want to deal with free variables, I could simply set the index of every
free variable to the first non-bound index at that level although this would have the effect of making all free variables be the
same free variable, regardless of whether or not they have the same variable name.

This doesn't matter either though since there is no way to do useful computation with "truely" free variables. (I.e. variables
which are free in the initial, top-level expression). So basically the system has the visual ids (the names) and the comptational
ids (the indecies). The visual ids for free variables work fine. The computational ids do not work fine but free variables can
never be used in computation anyway since they don't reference anything else in the expression and hence can never be used in
subtitution.

Once the indecies have been added to the variables, I can use the shift, sub and beta functions in the brujin example I wrote
to do beta reduction pretty easily. I think this is only defined for Lam (not Pi) but I will have to see. I also don't think
that the types in (Lam Name E E) (i.e. the first "E") are used in Beta reduction either. Certainly, since they are expressions
you could beta reduce them but I don't think it is necessary but we will also see about that too.

One more thing to mention is that since Nic is fairly convinced that the encoding to get show to correctly display different
types of functions (e.g. a -> b rather than Pi all the time) is very hard or impossible, I will simply have an indicator in the Pi
consturct itself which will be assigned during parsing. Simply put, if the expression is parsed as an arrow, then it will be
shown as an arrow. If it's parsed as a Pi, it will be shown as a Pi.

This brings me to my last thought which is that I need a consistent and fully funcitoning way to handle the underscore (empty
variable name) in Lams and Pis. Simply put, the underscore means that there are will be no references to the input of that function
in the function's output. Not only does this mean that you can write function types with the -> but it also means that during
beta reduction, you need only produce the output exactly as is, without performing any substitution or anything.

Finally, I really think I should test my brujin implementation on some other expressions before I assume that it is
correct because I had to make 2 odd changes to a mathematical definition of it in a paper. But I can't seem to find any
at the moment. 
-}
