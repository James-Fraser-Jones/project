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

--ex1: (λ λ 4 2 (λ 1 3)) (λ 5 1)
ex1 = App (Lam (Lam (App (App (Var 4) (Var 2)) (Lam (App (Var 1) (Var 3)))))) (Lam (App (Var 5) (Var 1)))
--this should beta reduce to: λ 3 (λ 6 1) (λ 1 (λ 7 1))
--                       not: λ 3 (λ 7 3) (λ 1 (λ 8 4))
-----------------------------------------------------------------------

inc :: Brexpr -> Int -> Index -> Brexpr
inc     (Var i) n k = Var $ i + (if i < k then 0 else n)
inc (App e1 e2) n k = App (inc e1 n k) (inc e2 n k)
inc    (Lam e1) n k = Lam $ inc e1 n (k+1)

sub :: Brexpr -> Index -> Brexpr -> Brexpr
sub (Var i) k e
  |  i < k          = Var i
  | i == k          = inc e k 0
  |  i > k          = Var $ i - 1
sub (App e1 e2) k e = App (sub e1 k e) (sub e2 k e)
sub    (Lam e1) k e = Lam $ sub e1 (k+1) e

beta :: Brexpr -> Brexpr
beta (App (Lam e1) e) = sub e1 1 e
beta e = e
