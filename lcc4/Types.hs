module Types where

import Data.Maybe
import Data.Either
import Data.List
import Data.Functor

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight (Left a) = error "Attempted to get Right value from a Left Either"

fromLeft :: Either a b -> a
fromLeft (Left a) = a
fromLeft (Right b) = error "Attempted to get Left value from a Right Either"

{-
I think that this form for expressions would neatly allow us to express (right associative) nested abstractions
and chains of (left associative) applications.

data Expr' = Abs Abs [(Var, Expr'')] Expr' deriving Eq
data Expr'' = Lit Lit | Var Var | App Expr' [Expr'] deriving Eq
-}

type Var = String

{-
I have removed the possibility for "empty" variable names in an attempt to make the language
as simple to reason about as possible
-}

data Expr = Lit Lit | Var Var | App Expr Expr | Abs Abs Var Expr Expr deriving Eq

data  Abs = Lam | Pi deriving Eq

data  Lit = Sort Sort | Type Type | Term Term deriving Eq
data Sort = Box | Star deriving Eq
data Type = Bool | Nat deriving (Eq, Show)
data Term = B Bool | N Int deriving Eq

{-
type Context = [(Var, Expr)]

data TypeError = BoxError --attempting to get type of Box
               | LookupError --attempting to get type of a free variable
               | MismatchAppError --attempting to apply an abstraction to an expression with the wrong type
               | NonAbsAppError --attempting to apply a non-abstraction to an exprression
               | NonSortError --expression is not a sort
--------------------------------------------------------------------------------------------------------
freeVars :: Expr -> [Var]
freeVars (Lit l) = []
freeVars (Var x) = [x]
freeVars (App e1 e2)  = (freeVars e1) `union` (freeVars e2)
freeVars (Abs _  Nothing e e') = (freeVars e) `union` (freeVars e')
freeVars (Abs _ (Just n) e e') = (freeVars e) `union` (freeVars e' \\ [n])

sub :: [Var] -> Expr -> Var -> Expr -> Expr
sub fVars s v (Lit l) = Lit l
sub fVars s v (Var x) = if x == v then s else Var x
sub fVars s v (App e1 e2) = App (sub fVars s v e1) (sub fVars s v e2)
sub fVars s v (Abs a Nothing e e') = Abs a Nothing (sub fVars s v e) (sub fVars s v e')
sub fVars s v (Abs a (Just n) e e')
  |          (n == v) = Abs a (Just n) e e'
  | (notElem n fVars) = Abs a (Just n) (sub fVars s v e) (sub fVars s v e') --can't understand this "rapier" method
  --the case where variables would be captured simply fails the pattern matching here

substitute :: Expr -> Bin -> Expr -> Expr
substitute _  Nothing e = e
substitute s (Just b) e = sub (freeVars s) s b e

--beta and nomalize functions just kind of brute-force the normal form of an expression
beta :: Expr -> Expr
--reduction
beta (App (Abs _ n _ e2) e) = substitute e n e2
--propagation
beta (App     e1 e2) = App     (beta e1) (beta e2)
beta (Abs a s e1 e2) = Abs a s (beta e1) (beta e2)
--neither reduction nor propagation
beta e = e

normalize :: Expr -> Expr
normalize e = if e == e' then e' else normalize e'
  where e' = beta e
--------------------------------------------------------------------------------------------------------
isSort :: Either TypeError Expr -> Either TypeError Expr
isSort (Right (Lit (Sort s))) = Right (Lit (Sort s))
isSort _ = Left NonSortError

extend :: Bin -> Expr -> Context -> Context
extend Nothing _ c = c
extend (Just n) e c = (n,e):c

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

--this function needs to check for alpha equivalence of the types, NOT syntactic equivalence
appComp :: Expr -> Expr -> Expr -> Either TypeError Expr
appComp (Abs Pi x t1 b) e t2 = if t1 == t2 then Right $ substitute e x b else Left MismatchAppError
appComp _ _ _ = Left NonAbsAppError

typeCheck :: Expr -> Either TypeError Expr
typeCheck = tC []

getType :: Expr -> Expr
getType = fromRight.typeCheck
--------------------------------------------------------------------------------------------------------
-}
