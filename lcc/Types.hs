module Types where

type Name = String

--data E = Var Name | Star | Box | App E E | Lam Name E E | Dep Name E E deriving Eq

data E = Lit Term | LitT Type | Var Name | Star | Box | App E E | Lam Name E E | Dep Name E E deriving Eq
--I think that Lam is for creating functions and Dep is simply for creating the types of functions.
--For instance, if you wanted to create a function that takes in types and returns types, there is no term-level representation of this
--So you start with "Lam typevariable : Star . type" and the type of this function itself is expressed by Dep
--so in this case, the type would be * -> * which is Dep _ Star Star.

--You can even create functions that take in Kinds themselves:

-- "Lam kindvariable : Box . kind" in this case a kind can be any *, * -> *, (* -> *) -> * etc..

data Term = B Bool | I Int deriving Eq

data Type = Bool | Int deriving Eq

type Context = [(E, E)]

{-
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

k ::= ∗ | Πx:A.k | Πa:k.k'
A ::= a | p | Πx:A.B | ∀a:k.A | Λx:A.B | A [e] | λa:k.A | A B
e ::= x | λx:A.e | e e | Λa:k.e | e [A]
-}
