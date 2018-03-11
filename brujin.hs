--https://pdfs.semanticscholar.org/15d1/fc255f683f132b24da82c1b162d2a105449e.pdf
--https://en.wikipedia.org/wiki/De_Bruijn_index

type Index = Int

data Brexpr = Var Index
            | App Brexpr Brexpr
            | Lam Brexpr

braced :: Brexpr -> Bool
braced (Var i) = False
braced _ = True

brace :: Brexpr -> String
brace x = if braced x then b (show x) else show x
  where b s = "(" ++ s ++ ")"

instance Show Brexpr where
  show (App (App e1 e2) e3) = (show (App e1 e2)) ++ " " ++ (brace e3)
  show (App e1 e2) = (brace e1) ++ " " ++ (brace e2)
  show (Lam e1) = "λ " ++ (show e1)
  show (Var i) = show i

--ex1: (λ λ 4 2 (λ 1 3)) (λ 5 1) beta reduces to: λ 3 (λ 6 1) (λ 1 (λ 7 1))
ex1 = App (Lam (Lam (App (App (Var 4) (Var 2)) (Lam (App (Var 1) (Var 3)))))) (Lam (App (Var 5) (Var 1)))

--ex2: 1 (λ 1 (λ 1 (λ 1 (λ 1))))
ex2 = App (Var 1) (Lam (App (Var 1) (Lam (App (Var 1) (Lam (App (Var 1) (Lam (Var 1))))))))

shift :: Index -> Brexpr -> Int -> Brexpr
shift k     (Var i) n = Var $ i + (if i > k then n else 0)
shift k (App e1 e2) n = App (shift k e1 n) (shift k e2 n)
shift k    (Lam e1) n = Lam $ shift (k+1) e1 n

--all free variables are shifted
shiftFree = shift 0

{-
This function substitutes all variables in expression, a, that reference the removed lam with an expression, e.
It also corrects free variables of e to avoid them being captured.
It also decrements the free variables of the a to correct them after the outer lam was removed (due to beta reduction).
Because of this, its functionality is only correct in the context of it being called from "beta" function.
-}
sub :: Brexpr -> Index -> Brexpr -> Brexpr
sub (Var i) k e
  |  i < k          = Var i             --do nothing to bound variables which reference a different lambda
  | i == k          = shiftFree e (k-1) --replace correct variables with e and shift free variables of e to prevent capture
  |  i > k          = Var $ i - 1       --decrement free variables to compensate for lambda being removed during beta reduction
sub (App e1 e2) k e = App (sub e1 k e) (sub e2 k e)
sub    (Lam e1) k e = Lam $ sub e1 (k+1) e

--performs beta reduction, if possible
beta :: Brexpr -> Brexpr
beta (App (Lam e1) e) = sub e1 1 e --"1" represents all variables which were bound by the Lam in "Lam e1"
beta e = e

{-
The first problem was that variables which were incremented were incremented 1 too many times.
Changing k to k-1 fixes this.
The second problem was that bound variables with an index of 1 were being incremented because of the
conditional logic in the shift function.
Changing 0 to 1 fixes this. As does changing "<" to "<=" in the shift function itself.

About debrujin indecies:

Lambda expressions represent binary trees with lambdas scattered throughout them.
Down any single branch of this tree there will be a certain number of lambdas along this path.
Any variable along this path can reference one of the lambdas above it (in which case it is bound).
Or it may reference a lambda in an imaginary path which extends higher than the tree itself (in which case it is free).

The (global) lambda that a variable references depends on the index (a positive, non-zero, integer),
AND the depth of lambdas that it exists underneath.
For instance, a variable "1" which exists at depth "1" (i.e. under 1 lambda) references the 1-1 (+1) = 1st lambda

A variable "1" which exists at depth "2" references the 2-1 (+1) = 2nd lambda

A variable "2" which exists at depth "1" references the (imaginary) 1-2 (+1) = 0th lambda (this variable is actually free)

A variable "2" which exists at depth "2" again references the first lambda.

So there are effectively global ids for lambdas and variables reference these with context specific indecies
which take their global depth into account.

It is possible to reference any lambda above a variable from any depth due to:
Lambda = Depth - Index

0 = 1-1 = 2-2 = 3-3 = 4-4 = 5-5 = 6-6 = 7-7 ...
-1 = 1-2 = 2-3 = 3-4 = 4-5

Technically speaking, it would be possible to reference lambdas beneath a variable if we used negative indecies, the
problem with this however is that the binary tree strcture means that application would make it impossible to know which
of the two possible branches to go down when we are looking for a lambda that the variable is referencing.

-}
