module Types where

import Data.Maybe
import Data.Either
import Data.List
import Data.Functor
--------------------------------------------------------------------------------------------------------
fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight (Left a) = error "Attempted to get Right value from a Left Either"

fromLeft :: Either a b -> a
fromLeft (Left a) = a
fromLeft (Right b) = error "Attempted to get Left value from a Right Either"
--------------------------------------------------------------------------------------------------------
type Var = String
type Context = [(Var, Expr)]

data Expr = Lit Lit | Var Var | App Expr Expr | Abs Abs Var Expr Expr deriving Eq

data  Abs = Lam | Pi deriving Eq --used to differentiate functions from function types

data  Lit = Sort Sort | Type Type | Term Term deriving Eq
data Sort = Box | Star deriving Eq --the 2 sorts used in the lambda cube language
data Type = Bool | Nat deriving (Eq, Show) --the types of the terms below
data Term = B Bool | N Int deriving Eq --the example literal terms of this language

data TypeError = BoxError --attempting to get type of Box
               | LookupError --attempting to get type of a free variable
               | MismatchAppError --attempting to apply an abstraction to an expression with the wrong type
               | NonAbsAppError --attempting to apply a non-abstraction to an exprression
               | NonSortError --expression is not a sort when it should be
--------------------------------------------------------------------------------------------------------
freeVars :: Expr -> [Var]
freeVars (Lit l) = []
freeVars (Var x) = [x]
freeVars (App e1 e2)  = (freeVars e1) `union` (freeVars e2)
freeVars (Abs _ v e e') = (freeVars e) `union` (freeVars e' \\ [v])

sub :: [Var] -> Expr -> Var -> Expr -> Expr
sub fVars s v (Lit l) = Lit l
sub fVars s v (Var v') = if v == v' then s else Var v'
sub fVars s v (App e1 e2) = App (sub fVars s v e1) (sub fVars s v e2)
sub fVars s v (Abs a v' e e')
  |          (v == v') = Abs a v' e e'
  | (notElem v' fVars) = Abs a v' (sub fVars s v e) (sub fVars s v e')
  --the case where variables would be captured simply fails the pattern matching here

substitute :: Expr -> Var -> Expr -> Expr
substitute s = sub (freeVars s) s

beta :: Expr -> Expr
beta (App (Abs _ v _ e2) e) = substitute e v e2 --reduction
beta (App     e1 e2) = App     (beta e1) (beta e2) --propagation
beta (Abs a v e1 e2) = Abs a v (beta e1) (beta e2)
beta e = e --neither reduction nor propagation

normalize :: Expr -> Expr
normalize e = if e == e' then e' else normalize e'
  where e' = beta e
--------------------------------------------------------------------------------------------------------
isSort :: Either TypeError Expr -> Either TypeError Expr
isSort (Right (Lit (Sort s))) = Right (Lit (Sort s))
isSort _ = Left NonSortError

extend :: Var -> Expr -> Context -> Context
extend v e c = (v,e):c

typeLit :: Lit -> Either TypeError Expr
typeLit l =
  case l of
    (Sort Box)   -> Left BoxError
    (Sort Star)  -> Right $ Lit (Sort Box)
    (Type _)     -> Right $ Lit (Sort Star)
    (Term (N _)) -> Right $ Lit (Type Nat)
    (Term (B _)) -> Right $ Lit (Type Bool)

tC :: Context -> Expr -> Either TypeError Expr
tC _ (Lit l) = typeLit l

tC c (Var x) = if isJust l then Right (fromJust l) else Left LookupError
  where l = lookup x c

tC c (Abs Lam x a b) = do
  b' <- (tC (extend x a c) b)
  let p = (Abs Pi x a b')
  isSort (tC c p) --check that p is "well typed"
  return p

tC c (Abs Pi x a b) = do
  isSort (tC c a) --check that a is "well typed"
  isSort (tC (extend x a c) b) --this gets returned

tC c (App e1 e2) = do
  f <- (tC c e1) --e1 should be a lambda abstraction, so f should be a pi abstraction
  t <- (tC c e2)
  appComp f e2 t

{-
I think that this function needs to check for alpha equivalence of the types, NOT syntactic equivalence
I'm also not sure whether or not beta equivalence should count here
-}
appComp :: Expr -> Expr -> Expr -> Either TypeError Expr
appComp (Abs Pi x t1 b) e t2 = if t1 == t2 then Right $ substitute e x b else Left MismatchAppError
appComp _ _ _ = Left NonAbsAppError

typeCheck :: Expr -> Either TypeError Expr
typeCheck = tC []
--------------------------------------------------------------------------------------------------------

{-
Example: run $ polyId ++ " @ Nat @ 6"

I think that this form for expressions would neatly allow us to express (right associative) nested abstractions
and chains of (left associative) applications.

data Expr' = Abs Abs [(Var, Expr'')] Expr' deriving Eq
data Expr'' = Lit Lit | Var Var | App Expr' [Expr'] deriving Eq

I have removed the possibility for "empty" variable names in an attempt to make the language
as simple to reason about as possible

---------------------------------------------------------------------------------------------------------

I need to figure out under exactly what circumstances certain (alpha and/or beta equivalences)
should be permitted and how to implement alpha equivalence with as little unneccesary computation
as possible

I also need to figure out how to do (named) capture avoidance with as little computation as possible
(rapier method is a possibility)

I also want to figure out how I can consistently support the ability to not use a binder in an
abstraction when it will not be used in the output expression

I also want to figure out how I can support both nested abstractions and nested applications
(right and left associativity, respectively) in a consistent way, particularly since expressions like
"\x,y -> x" aren't currently supported.

I also need to figure out how to implement addition and boolean conjunction as example functions to work
with my pre-defined literal values
-}
