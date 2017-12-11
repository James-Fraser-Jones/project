import Data.Maybe
import Data.Set
import qualified Data.Set as Set

type Name = String --to distinguish variable names from other strings

data Expr
  = Var Name      -- If x is a variable , then x ∈ Λ                     (variables)
  | App Expr Expr -- If (M, N ∈ Λ) , then (M N) ∈ Λ                      (application)
  | Lam Name Expr -- If x is a variable and M ∈ Λ , then (λx.M) ∈ Λ      (lambda abstraction)
  | Lit Name
  deriving (Eq)

instance Show Expr where
  --application is left associative (don't brace variables)
  show (App (App (e1) (e2)) (Var s)) = show (App (e1) (e2)) ++ " " ++ show (Var s)
  show (App (App (e1) (e2)) e3) = show (App (e1) (e2)) ++ " " ++ brace (show e3)
  --don't brace variables in applications
  show (App (Var s1) (Var s2)) = show (Var s1) ++ " " ++ show (Var s2)
  show (App e (Var s)) = brace (show e) ++ " " ++ show (Var s)
  show (App (Var s) e) = show (Var s) ++ " " ++ brace (show e)
  --brace application expressions by default
  show (App e1 e2) = brace (show e1) ++ " " ++ brace (show e2)
  --standard
  show (Lam s e) = "\\" ++ s ++ "." ++ show e
  show (Var s) = s
  show (Lit s) = s

brace :: String -> String
brace s = "(" ++ s ++ ")"
-----------------------------------------------------------------------------------------
fvars :: Expr -> Set Name --get a set containing every instance of a free variable in the expression
fvars (Lit s) = empty
fvars (Var s) = singleton s
fvars (App e1 e2) = Set.union (fvars e1) (fvars e2)
fvars (Lam s e) = Set.delete s (fvars e)

fvlist :: Expr -> [Name] --get list of unique free variable names of the expression
fvlist = toList.fvars
-----------------------------------------------------------------------------------------
getIndex :: [a] -> Int -> Maybe a
getIndex (x:xs) n = if n == 0 then Just x else getIndex xs (n-1)
getIndex [] _ = Nothing

fresh' :: Int -> Int -> Name
fresh' a n
    | (n <= 25) = [(fromJust (getIndex ['a'..'z'] n))] ++ (if a > 1 then show a else "")
    | otherwise = fresh' (a+1) (n-26)

fresh :: Int -> Name --get a fresh variable name
fresh = fresh' 1

unique' :: Int -> [Name] -> Name
unique' n nm = if elem (fresh n) nm then unique' (n+1) nm else fresh n

unique :: [Name] -> Name --get a unique variable name from the list given
unique = unique' 0
-----------------------------------------------------------------------------------------
sub :: Expr -> Name -> Expr -> Expr --substitute the free variables with name s' in the first expression with e' (capture avoiding)
sub (Lit s) _ _ = Lit s
sub (Var s) s' e' = if s == s' then e' else Var s
sub (App e1 e2) s' e' = App (sub e1 s' e') (sub e2 s' e')
sub (Lam s e) s' e'
  | (s == s')                = Lam s e               --s' variables become bound so don't sub within this scope
  | not (elem s (fvlist e')) = Lam s (sub e s' e')   --s' variables remain free so do sub within this scope
  -- | otherwise                = Lam s (sub e s' e')
  --{-
  | otherwise                = Lam us (sub ue s' e') --some free variables in e' will be captured by s binder wherever e' is subbed in
    where us = (unique.toList) (union (fvars e) (fvars e')) --therefore alpha rename s binder and its variables to avoid all free variable names in e' and e
          ue = sub e s (Var us) --}
-----------------------------------------------------------------------------------------
beta :: Expr -> Expr --single step of beta reduction
beta (App (Lam s e1) e2) = sub e1 s e2
beta (App e1 e2) = App (beta e1) (beta e2)
beta (Lam s e) = Lam s (beta e)
beta (Var s) = Var s
beta (Lit s) = Lit s
-----------------------------------------------------------------------------------------
eta :: Expr -> Expr --single step of eta reduction
eta (Lam s (App e (Var s'))) = if and [s == s', not (elem s (fvlist e))] then e else Lam s (eta (App e (Var s')))
eta (App e1 e2) = App (eta e1) (eta e2)
eta (Lam s e) = Lam s (eta e)
eta (Var s) = Var s
eta (Lit s) = Lit s
-----------------------------------------------------------------------------------------
evaluate :: Expr -> IO ()
evaluate e = do
  print e
  if beta e == e then (if eta e == e then return () else evaluate (eta e)) else evaluate (beta e)
-----------------------------------------------------------------------------------------
--evaluate test4
test4 = Lam "x" (Lam "y" (App (Lam "z" (App (Lam "x" (App (Var "z") (Var "x"))) (Lam "y" (App (Var "z") (Var "y"))))) (App (Var "x") (Var "y"))))
test5 = Lam "x" (Lam "y" (App (Lam "z" (App (Lam "x" (App (App (Lam "x" (Var "x")) (Var "z")) (Var "x"))) (Lam "y" (App (Var "z") (Var "y"))))) (App (Var "x") (Var "y"))))
test6 = Lam "x" (Lam "y" (App (Lam "z" (App (Lam "x" (App (App (Lam "x" (Var "x")) (Var "z")) (Var "x"))) (Lam "y" (App (Var "z") (Var "y"))))) (App (Var "a") (Var "y"))))
test7 = Lam "x" (Lam "y" (App (Lam "z" (App (Lam "x" (App (App (Lam "x" (Var "x")) (Var "z")) (Var "x"))) (Lam "y" (App (Var "z") (Var "y"))))) (App (Var "b") (Var "y"))))

test8 = App (Lam "x" (Lam "y" (App (App (Var "z") (Var "x")) (Lam "u" (App (Var "u") (Var "x")))))) (Lam "x" (App (Var "w") (Var "x"))) --example as expr

test9 = App (App (App (App (App (Var "u") (Var "v")) (Var "w")) (Var "x")) (Var "y")) (Var "z")
test10 = App (Var "z") (App (App (App (Var "w") (App (Var "u") (Var "v"))) (Var "x")) (Var "y"))

test11 = Lam "a" (App (Var "b") (Var "a")) --capture avoidance tests
test12 = Lam "a" (App (App (Var "b") (Var "a")) (Lam "c" (App (App (Var "a") (Var "b")) (Var "c"))))
test13 = Lam "a" (App (App (Var "b") (Var "a")) (Lam "c" (App (App (App (Var "a") (Var "b")) (Var "c")) (Lam "d" (App (App (App (Var "a") (Var "b")) (Var "c")) (Var "d"))))))
