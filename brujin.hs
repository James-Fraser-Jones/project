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

--this increments all the "free" variables within an expression, the higher k is, the fewer variables are considered free
shift :: Brexpr -> Int -> Index -> Brexpr
shift     (Var i) n k = Var $ i + (if i < k then 0 else n)
shift (App e1 e2) n k = App (shift e1 n k) (shift e2 n k)
shift    (Lam e1) n k = Lam $ shift e1 n (k+1)

{-
This function substitutes all variables in expression, a, that reference the removed lam with an expression, e.
It also corrects free variables of e to avoid them being captured.
It also decrements the free variables of the a to correct them after the outer lam was removed (due to beta reduction).
Because of this, its functionality is only correct in the context of it being called from "beta" function.
-}
sub :: Brexpr -> Index -> Brexpr -> Brexpr
sub (Var i) k e
  |  i < k          = Var i
  | i == k          = shift e (k-1) 1 --this had to be changed from "inc e k 0" to correct two seperate problems
  |  i > k          = Var $ i - 1
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
-}
