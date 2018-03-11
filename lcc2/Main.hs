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

{-
Something I've been thinking about:
(->) should really be considered to be syntactically eqivalent to (.)
Haskell actually uses -> instead of . in its anonymous (lambda) functions anyway.
And in this way it means that Lams look exactly the same as Pis with the only difference being that
\x:a -> E is a function from x to E and Π:x:a -> A is (effectively) a function from a to A

With this in mind, I actually think that for the sake of simplicity, the syntax should be as restrictive as possible
in which case the (.) should be completely removed in favor of (->).

If both Lams and Deps accept (_) empty variable names then
\a -> E means \_:a -> E
and
Πa -> A means Π_:a -> A

the obvious problem with this syntax is that the type of term-term and type-type functions does not typically include the
Pi at the start but this would be necessary to distinguish the Pis from the Lams since the lams have become more expressive
with their ability to express functions that take an input of type a and deliver a constant output expression E using the
syntax \a -> E

The only slightly annoying thing is that it isn't possible to have a function that may take an input from any type and
output a constant expression. This isn't a problem that is to do with the syntax however, it is due to the undecideability
of type inference in CoC as a whole.

A possibility could be to have an equivalent of empty variable names: the empty type which would just be a special case of
expression maybe??

Anyway it would look like this:

\~ -> E means \_:~ -> E which is a function that takes ANY Expression as its type (since the type of the unreferenced variable
doesn't matter) and outputs E.

Perhaps _ could be used to represent both but they would mean distinctly different things. Using _ as a varible name means
that there will be no instance of that name in the output expression and hence no substitution is necessary in beta reduction.
The ~ means that any all types should satisfy the type checker here because it doesn't matter what the type of variable _ is
when it will never be substituted into the body of E.

Seems a bit dangerous to add this just yet but it's a possibility

Lams and Pis should take the following to represent the possibility of empty variables:
Lam (Maybe Var) Expr Expr where Nothing represents an empty variable

\Int -> True is a function that takes an Integer and returns the constant expression True
(\(t:*) -> (\t -> True)) Int 6 = (\(t:*) -> (\(_:t) -> True)) Int 6
(\Int -> True) 6 = (\(_:Int) -> True) 6
True

There is definitely a lot of symetry in the syntax here that is not being taken advantage of for the sake of
clarity in my opinion.
-}
