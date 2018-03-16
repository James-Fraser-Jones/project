module Types where

import Data.Maybe
import Data.List
import Data.Functor

type Name = String

data Expr = Lit Lit | Var Name | App Expr Expr | Lam (Maybe Name) Expr Expr | Pi (Maybe Name) Expr Expr deriving Eq
data  Lit = Sort Sort | Type Type | Term Term deriving Eq
data Sort = Box | Star deriving Eq
data Type = Bool | Nat deriving (Eq, Show)
data Term = B Bool | N Int deriving Eq

vars :: [Name]
vars = (\(a, b) -> ['a'..'z'] !! b : if a == 0 then "" else show $ a+1).(flip quotRem 26) <$> [0..]

fresh :: [Name] -> Name --given a set of used variable names, return a fresh variable from an infinite list
fresh s = head $ filter (flip notElem s) vars

freeVars :: Expr -> [Name]
freeVars (Lit l) = []
freeVars (Var x) = [x]
freeVars (App e1 e2)  = (freeVars e1) `union` (freeVars e2)
freeVars (Lam  Nothing e e') = (freeVars e) `union` (freeVars e')
freeVars (Lam (Just n) e e') = (freeVars e) `union` (freeVars e' \\ [n])
freeVars (Pi   Nothing e e') = (freeVars e) `union` (freeVars e')
freeVars (Pi  (Just n) e e') = (freeVars e) `union` (freeVars e' \\ [n])

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
-}

sub :: [Name] -> Name -> Expr -> Expr -> Expr
sub fVars v s (Lit l) = Lit l
sub fVars v s (Var x) = Var $ if x == v then v else x
sub fVars v s (App e1 e2) = App (sub fVars v s e1) (sub fVars v s e2)
sub fVars v s (Lam  Nothing e e') = Lam Nothing (sub fVars v s e) (sub fVars v s e')
sub fVars v s (Lam (Just n) e e')
  | (n == v) = Lam (Just n) e e'
  | (notElem n fVars) = Lam (Just n) (sub fVars v s e) (sub fVars v s e') --can't understand this "rapier" paper

substitute :: Name -> Expr -> Expr -> Expr
substitute v s e = sub (freeVars s) v s e
