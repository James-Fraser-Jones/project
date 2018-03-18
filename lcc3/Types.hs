module Types where

import Data.Maybe
import Data.Either hiding (show)
import Data.List
import Data.Functor

fromRight e = head $ rights [e]
fromLeft e = head $ lefts [e]

type Var = String
type Bin = Maybe Var
type Context = [(Var, Expr)]

data Expr = Lit Lit | Var Var | App Expr Expr | Abs Abs Bin Expr Expr deriving Eq

data  Abs = Lam | Pi deriving Eq
data  Lit = Sort Sort | Type Type | Term Term deriving Eq

data Sort = Box | Star deriving Eq
data Type = Bool | Nat deriving (Eq, Show)
data Term = B Bool | N Int deriving Eq

data TypeError = BoxError
               | LookupError
               | MismatchAppError
               | NonAbsAppError
               | NonWellFormedError
-----------------------------------------------------------------------------------------------
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

(===) :: Expr -> Expr -> Bool --beta equivalence
e1 === e2 = (normalize e1) == (normalize e2)
-----------------------------------------------------------------------------------------------
isSort :: Either TypeError Expr -> Either TypeError Expr
isSort (Right (Lit (Sort s))) = Right (Lit (Sort s))
isSort _ = Left NonWellFormedError

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

typeCheck :: Expr -> Either TypeError Expr
typeCheck = tC []

tC :: Context -> Expr -> Either TypeError Expr
tC _ (Lit l) = typeLit l
tC c (Var x) = if isJust l then Right (fromJust l) else Left LookupError
  where l = lookup x c

tC c (Abs Pi x a b) = do
  s <- isSort (tC c a) --this should not be ignored if it errors, even though it is not present in the return statement
  t <- isSort (tC (extend x a c) b)
  return t

tC c (Abs Lam x a b) = do
  b' <- (tC (extend x a c) b)
  t  <- isSort (tC c (Abs Pi x a b'))
  return (Abs Pi x a b')

--getting application to work is a nightmare, I feel like the best way might be to just recurse through the entire structure once
--and then beta reduce once that's all done but I can't seem to figure out how that should be done
--actually I think all I really need to do is when it recurses through the structure initially it should only perform
--the tC recursion on the left hand expression: tc (app (app e1 e2) e3) = app (tc (app e1 e2)) e3 = app (app (tc e1) e2) e3
--this would certainly work if (during normalization of the resulting expression) there was type checking for the
--case of mismatchapperror and NonAbsAppError

tC c (App f a) = do
  f' <- (tC c f)
  a' <- (tC c a)
  appComp f' a'

appComp :: Expr -> Expr -> Either TypeError Expr
appComp (Abs Pi x a' b') a = if a == a' then Right $ substitute a x b' else Left $ MismatchAppError
appComp _ _ = Left NonAbsAppError
-----------------------------------------------------------------------------------------------

{-
The implication above is that given (\x:e.e') the "lambda x" DOES bind the free x's in e' but DOES NOT
bind the free x's in e. However, given a nested expression: (\x:e.(\y:e'.e'')) the free x's in e' ARE
bound. This is what we want as it allows us, for example, to make the polymorphic identity function on terms:
(\x:*.\y:x.y) if the x in the first lambda didn't bind the type "x" in the second expression, then x would remain
a free variable in this expression (\y:x.y) which wouldn't type check.

Paper talks about domain and range of substitution.
Substitution as a function looks like this: e[x := e']
Which is a function like this: Expr x Var x Expr -> Expr

The domain of a function is the set of possible different inputs. The range is the set of possible outputs.
In this case it is clear that the output of the function is the Expression produced from the substitution.
However, since there are 2 seperate expressions in the input, it is not clear which should be considered when
talking about the domain of the function.

vars :: [Name]
vars = (\(a, b) -> ['a'..'z'] !! b : if a == 0 then "" else show $ a+1).(flip quotRem 26) <$> [0..]

fresh :: [Name] -> Name --given a set of used variable names, return a fresh variable from an infinite list
fresh s = head $ filter (flip notElem s) vars
-}
