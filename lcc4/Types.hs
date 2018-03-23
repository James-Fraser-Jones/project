module Types where
--------------------------------------------------------------------------------------------------------
--Either Functions

isRight (Right _) = True
isRight _ = False
isLeft = not.isRight
fromRight (Right b) = b
fromRight (Left a) = error "Attempted to get Right value from a Left Either"
fromLeft (Left a) = a
fromLeft (Right b) = error "Attempted to get Left value from a Right Either"
--------------------------------------------------------------------------------------------------------
--Types

type Var = String
type Context = [(Var, Expr)]
type Calculus = [(Sort, Sort)]

data Expr = Lit Lit | Var Var | App Expr Expr | Abs Abs Var Expr Expr deriving Eq

data  Abs = Lam | Pi deriving Eq --used to differentiate functions from function types

data  Lit = Sort Sort | Type Type | Term Term deriving Eq
data Sort = Box | Star deriving Eq --the 2 sorts used in the lambda cube language
data Type = Bool | Nat deriving (Eq, Show) --the types of the terms below
data Term = B Bool | N Int deriving Eq --the example literal terms of this language

data ParseError = RemainError --not all of the input was consumed during parsing
                | GeneralError --an error occoured during parsing

data TypeError = BoxError --attempting to get type of Box
               | LookupError --attempting to get type of a free variable
               | MismatchAppError --attempting to apply an abstraction to an expression with the wrong type
               | NonLamAppError --attempting to apply a non-lambda abstraction to an exprression
               | NonSortError --expression is not a sort when it should be
