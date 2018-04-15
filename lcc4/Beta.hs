module Beta (delta, normalize, typeCheck) where
import Types

import Data.List
import Data.Maybe
import Data.Functor
--------------------------------------------------------------------------------------------------------
--Alpha equivalence

canonym' :: Cantext -> String -> Expr -> Expr --alternatively, just convert both expressions to use de brujin indecies
canonym' _ _ (Lit l) = Lit l
canonym' c _ (Var x) = Var $ if isJust l then fromJust l else "F" --all (globally) free variables are alpha equivalent
  where l = lookup x c
canonym' c s (App e1 e2) = App (canonym' c ('L':s) e1) (canonym' c ('R':s) e2)
canonym' c s (Abs a v e e') = Abs a new (canonym' c s e) (canonym' ((v,new):c) new e')
  where new = ('N':s)

canonym :: Expr -> Expr
canonym = canonym' [] "P" --only lowercase variable names can be parsed so "P" can't capture

alpha :: Expr -> Expr -> Bool
alpha e1 e2 = (canonym e1) == (canonym e2)
--------------------------------------------------------------------------------------------------------
--Beta reduction

allVars :: Expr -> [Var]
allVars (Lit l) = []
allVars (Var x) = [x]
allVars (App e1 e2)  = (allVars e1) `union` (allVars e2)
allVars (Abs _ v e e') = (allVars e) `union` (allVars e')

freeVars :: Expr -> [Var]
freeVars (Lit l) = []
freeVars (Var x) = [x]
freeVars (App e1 e2)  = (freeVars e1) `union` (freeVars e2)
freeVars (Abs _ v e e') = (freeVars e) `union` (freeVars e' \\ [v])

vars :: [Var]
vars = (\(a, b) -> ['a'..'z'] !! b : if a == 0 then "" else show $ a+1).(flip quotRem 26) <$> [0..]

fresh :: [Var] -> Var
fresh used = head $ vars \\ used

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
--------------------------------------------------------------------------------------------------------
--Typechecking

isSort :: Expr -> Either Error Expr
isSort (Lit (Sort s)) = Right $ Lit (Sort s)
isSort _ = Left NonSortError

wellTyped :: AbsForms -> Expr -> Expr -> Either Error Expr
wellTyped ca (Lit (Sort i)) (Lit (Sort o)) =
  if elem (i, o) ca then Right (Lit (Sort o)) else Left NonSortError
wellTyped _ _ _ = Left NonSortError

extend :: Var -> Expr -> Context -> Context
extend v e c = (v,e):c

typeLit :: Lit -> Either Error Expr
typeLit l =
  case l of
    (Sort Box)   -> Left BoxError
    (Sort Star)  -> Right $ Lit (Sort Box)
    (Type _)     -> Right $ Lit (Sort Star)
    (Term (N _)) -> Right $ Lit (Type Nat)
    (Term (B _)) -> Right $ Lit (Type Bool)
    (Func And)   -> Right $ Abs Pi "x" (Lit (Type Bool)) (Abs Pi "y" (Lit (Type Bool)) (Lit (Type Bool)))
    (Func Plus)  -> Right $ Abs Pi "x" (Lit (Type Nat)) (Abs Pi "y" (Lit (Type Nat)) (Lit (Type Nat)))

tC :: Context -> AbsForms -> Expr -> Either Error Expr
tC _ _ (Lit l) = typeLit l

tC c ca (Var x) = if isJust l then Right (fromJust l) else Left LookupError
  where l = lookup x c

tC c ca (Abs Lam x a b) = do
  b' <- (tC (extend x a c) ca b)
  let p = (Abs Pi x a b')
  p' <- (tC c ca p)
  isSort p'
  return p

tC c ca (Abs Pi x a b) = do
  a' <- (tC c ca a)
  b' <- (tC (extend x a c) ca b)
  wellTyped ca a' b'

tC c ca (App e1 e2) = do
  f <- (tC c ca e1) --e1 should be a lambda abstraction, so f should be a pi abstraction
  t <- (tC c ca e2)
  appComp f e2 t

appComp :: Expr -> Expr -> Expr -> Either Error Expr
appComp (Abs Pi x t1 b) e t2 = if alpha t1 t2 then Right $ substitute e x b else Left MismatchAppError
appComp _ _ _ = Left NonLamAppError
--------------------------------------------------------------------------------------------------------
--Delta Reduction (applying literal functions to literal values)

getInt :: Expr -> Int
getInt (Lit (Term (N n1))) = n1
getInt (App (App (Lit (Func Plus)) n1) n2) = (getInt n1) + (getInt n2)

getBool :: Expr -> Bool
getBool (Lit (Term (B b1))) = b1
getBool (App (App (Lit (Func And)) b1) b2) = (getBool b1) && (getBool b2)

delta :: Expr -> Expr -> Expr
delta (Lit (Type Nat)) e = (Lit (Term (N $ getInt e)))
delta (Lit (Type Bool)) e = (Lit (Term (B $ getBool e)))
delta _ e = e
--------------------------------------------------------------------------------------------------------
--Top level functions

normalize :: Expr -> Expr
normalize e = if e == e' then e' else normalize e'
  where e' = beta e

typeCheck :: AbsForms -> Expr -> Either Error Expr
typeCheck = tC [] --begin typechecking with empty context
--------------------------------------------------------------------------------------------------------
--Notes

{-
Example: run $ polyId ++ " @ Nat @ 6"

I think that this form for expressions would neatly allow us to express (right associative) nested abstractions
and chains of (left associative) applications.

data Expr' = Abs Abs [(Var, Expr'')] Expr' deriving Eq
data Expr'' = Lit Lit | Var Var | App Expr' [Expr'] deriving Eq

I have removed the possibility for "empty" variable names in an attempt to make the language
as simple to reason about as possible

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
--------------------------------------------------------------------------------------------------------
VARIABLE CAPTURE:

I do need capture avoiding substitution:
"\\y:Nat -> ((\\x:Nat -> \\y:Nat -> x) @ y @ 2)"
beta reduces to:
"\\y:Nat -> ((           \\y:Nat -> y)     @ 2)"
then to:
"\\y:Nat ->                         2          "

What should have happened was:
"\\y:Nat -> ((\\x:Nat -> \\z:Nat -> x) @ y @ 2)"
beta reduces to:
"\\y:Nat -> ((           \\z:Nat -> y)     @ 2)"
then to:
"\\y:Nat ->                         y          "

This is in spite of the fact that
"(\\y:Nat -> ((\\x:Nat -> \\y:Nat -> x) @ y @ 2)) @ 5"
actually correctly reduces to 5, not 2 as suggested by the unapplied function
however this is still variable capture that needs to be dealt with

LITERALS:

I don't really know how to implement literals propperly. Can't figure out a propper way to implement conditional
branches (ifs, case statements, guards, pattern matching, etc..)
Would also be nice to add definitions and ADTs but I think this is going to far really.

UTILITY:

Get command line interface working propperly (maybe I can use Haskeline??)
all I really need is for arrow keys to exhibit their usual behavior with regards to the command line.

EXTRAS:

Maybe change error messages to reflect when someone has attempted to use a more powerful calculus than is allowed?

Consider the language extentions you were talking about which would allow for more easy nesting of abstractions
and applications and empty variables etc..
-}
