> module Types where

> type Name = String
> --data E = Var Name | Star | Box | App E E | Lam Name E E | Dep Name E E deriving Eq
> data E = Lit Term | LitT Type | Var Name | Star | Box | App E E | Lam Name E E | Dep Name E E deriving Eq
> data Term = B Bool | I Int deriving Eq
> data Type = Bool | Int deriving Eq
> type Context = [(E, E)]

k ::= ∗
A ::= p | A → B
e ::= x | λx:A.e | e e

k ::= ∗
A ::= a | p | A → B | ∀a:k.A
e ::= x | λx:A.e | e e | Λa:k.e | e [A]

k ::= ∗ | k → k
A ::= a | p | A → B | ∀a:k.A | λa:k.A | A B
e ::= x | λx:A.e | e e | Λa:k.e | e [A]

k ::= ∗ | Πx:A.k | Πa:k.k'
A ::= a | p | Πx:A.B | ∀a:k.A | Λx:A.B | A [e] | λa:k.A | A B
e ::= x | λx:A.e | e e | Λa:k.e | e [A]

T ::= ☐
k ::= ∗ |                                   Πx:A.k | Πx:k.k'
A ::= p | x | λx:A.B | λx:k.A | A e | A B | Πx:A.B | Πx:k.A
e ::= l | x | λx:A.e | λx:k.e | e e | e A
     Lit|Var|       Abs       |    App    |       Dep

So we have:

"Types" of the 4 different functions:
Πx:A.k | Πx:k.k'
Πx:A.B | Πx:k.A

The creation of the 4 different functions through abstraction:
λx:A.B | λx:k.A
λx:A.e | λx:k.e

The resolution of the 4 different functions through application:
A e | A B
e e | e A

Type and term variables: x (kind variables aren't used because we don't have functions from kinds to other types/terms/kinds)

Literal for top:    ☐
Literal for kinds:  *
Literals for types: p
Literals for terms: l

-----------------------------------------------------------------------
In detail:
Term to term function: λx:A.e (normal function, term level)
                 Type: Πx:A.B
                  App: e e

Type to term function: λx:k.e (polymorphism, term level)
                 Type: Πx:k.A
                  App: e A

Type to type function: λx:k.A (type operator, type level)
                 Type: Πx:k.k'
                  App: A B

Term to type function: λx:A.B (dependent type, type level)
                 Type: Πx:A.k
                  App: A e
------------------------------------------------------------------------
Another thing that I think:

Since: Πx:A.B = A -> B and A -> B : *, Πx:A.B : * : ☐
                                  and  Πx:k.k': ☐ (for the same reason)

But also: Πx:k.A = ∀x:k.A (but I don't know the type of this).

Finally, Πx:A.k has no equivalence to previous constructs and I still don't know the type of this.

Maybe we aren't technically supposed to find the types of types of functions?
------------------------------------------------------------------------
For a lambda abstraction, the type of the function that it creates is always in the above level.
Since we allow 2 different lambda abstractions at the Term level: Term to Term and Type to Term,
We therefore allow 2 different dep abstractions at the Type level to represent the type of each of these abstractions.

Similarly, we allow 2 different lambda abstractions at the Type level: Type to Type and Term to Type,
so we allow 2 different dep abstractions at the Kind level to represent the type of each of these abstractions.

Remember: "type" is representation of a construct in the level above
          "Type" is the 2nd of the 4 levels: Term, Type, Kind, Top
------------------------------------------------------------------------
T ::= ☐
k ::= ∗ |                                      Πx:A.k  | Π_:k.k'
A ::= p | a | λx:A.A' | λa:k.A | A e  | A A' | Π_:A.A' | Πa:k.A
e ::= l | x | λx:A.e  | λa:k.e | e e' | e A
     Lit|Var|        Abs       |     App     |        Dep

I rewrote the formulas to make it clear that A and A' are (potentially) different types, as are e and e' and k and k' for terms and kinds respectively.
------------------------------------------------------------------------
There is an odd inconsistency in the way you read abs and dep in spite of them having the same syntax.

λa:k.A = "This function takes in a value with type k to produce an A".
"k is a Kind, hence a is a Type, A is also a Type therefore the function is Type to Type".

Πa:k.k' = "This is the type of a function that takes in an 'a' (of type k) and produces a value (of type k')".
"since k and k' are both kinds, a is a Type and the value it outputs is also a Type therefore the function is Type to Type".

Put more clearly: λ(variable):(type).(output value)
                  Π(variable):(type).(type of output value)

Hence, the corresponding Π as the type of a λ will always have its output expression at 1 level higher than the λ's output expression
e.g. type to type function, output = type, λ output = type, Π output = type of (λ output) = type of type = * = kind
'
------------------------------------------------------------------------
Notice above that even without the abs, dep and app required for dependent types (i.e. removing λx:A.A', Πx:A.k and A e), at the type level we have:
A ::= p | a | λa:k.A | A A' | Π_:A.A' | Πa:k.A

We understand that the two remaining deps are only used to service the term level below so we can remove them too (Π_:A.A' and Πa:k.A) Now we have:

A ::= p | a | λa:k.A | A A' which is a STLC at the type level.

We can even show that the kind level above satisfies the STLC's type requirements:

k ::= ∗ | Π_:k.k' which equals: k ::= ∗ | k -> k' (base and function types where the type of all propper types is * )

This proves that lambda omega has an STLC at the type level due to its ability to express functions from types to types.
-------------------------------------------------------------------------
I can also see from this structure that there isn't really any sensible type for Πx:A.k nor for Πa:k.A = ∀x:k.A.

Why? It's because for an expression to have a type, there must be some construct in the level above to represent it.
In the case of Π_:A.A' = A -> A', the type is * (because A -> A' is a function type). It also happens that the kind level
only has 3 possibilities. 2 of them are deps which only leaves the literal *.

It the same case with Π_:k.k' = k -> k'. The type (by definition) of this function is ☐ but this also happens to be the only thing it could be
since the top level only has one expression which is exactly ☐.

So unless the type of EVERYTHING at the kind level is also supposed to be ☐, there is no sensible type for Πx:A.k.

Likewise, unless the type of ∀x:k.A. is ∗, Πx:A.k or Π_:k.k' there is no sensible type for it either.
