import Data.Functor
import Data.Set (Set)
import qualified Data.Set as Set
-----------------------------------------------------------------------------------------
--this is 100% correct, inclusion of literals here is purely optional
type Name = String --to distinguish variable names from other strings
data Expr
  = Var Name      -- If x is a variable , then x ∈ Λ                     (variables)
  | App Expr Expr -- If (M, N ∈ Λ) , then (M N) ∈ Λ                      (application)
  | Lam Name Expr -- If x is a variable and M ∈ Λ , then (λx.M) ∈ Λ      (lambda abstraction)
  | Lit Name
  deriving (Eq)
-----------------------------------------------------------------------------------------
--both of these functions do exactly what they're supposed to do
fvars :: Expr -> Set Name --get a set containing every instance of a free variable in the expression
fvars (Lit s) = Set.empty
fvars (Var s) = Set.singleton s
fvars (App e1 e2) = Set.union (fvars e1) (fvars e2)
fvars (Lam s e) = Set.delete s (fvars e)

fresh :: Set Name -> Name --given a set of used variable names, return a fresh variable from an infinite list
fresh s = head $ filter (flip Set.notMember s) vars

vars :: [Name]
vars = (\(a, b) -> ['a'..'z'] !! b : if a == 0 then "" else show $ a+1).(flip quotRem 26) <$> [0..]
-----------------------------------------------------------------------------------------
--capture avoiding substitution function works as it's supposed to, evidenced by stack exchange answer on the subject
sub :: Expr -> Name -> Expr -> Expr --substitute the free variables with name s' in the first expression with e' (capture avoiding)
sub (Lit s) _ _ = Lit s
sub (Var s) s' e' = if s == s' then e' else Var s
sub (App e1 e2) s' e' = App (sub e1 s' e') (sub e2 s' e')
sub (Lam s e) s' e'
  | (s == s')                  = Lam s e               --s' variables become bound so don't sub within this scope
  | Set.notMember s $ fvars e' = Lam  s (sub  e s' e') --s' variables remain free so do sub within this scope
  | otherwise                  = Lam us (sub ue s' e') --some free variables in e' will be captured by s binder wherever e' is subbed in
    where us = fresh $ Set.union (fvars e) (fvars e')  --therefore alpha rename s binder and its variables to avoid all free variable names in e' and e
          ue = sub e s (Var us)
-----------------------------------------------------------------------------------------
--UGLY BELOW
-----------------------------------------------------------------------------------------

--all these functions make a bunch of assumptions about the evaluation strategy being used
--which hasn't been propperly defined although the first rule of beta is certainly correct
--and the first rule of eta redction should probably also be correct too, I need to check this
beta :: Expr -> Expr --single step of beta reduction
beta (App (Lam s e1) e2) = sub e1 s e2
beta (App e1 e2) = App (beta e1) (beta e2)
beta (Lam s e) = Lam s (beta e)
beta (Var s) = Var s
beta (Lit s) = Lit s

eta :: Expr -> Expr --single step of eta reduction
eta (Lam s (App e (Var s'))) = if and [s == s', Set.notMember s $ fvars e] then e else Lam s (eta (App e (Var s')))
eta (App e1 e2) = App (eta e1) (eta e2)
eta (Lam s e) = Lam s (eta e)
eta (Var s) = Var s
eta (Lit s) = Lit s

evaluate :: Expr -> IO ()
evaluate e = do
  print e
  if beta e == e then (if eta e == e then return () else evaluate (eta e)) else evaluate (beta e)
-----------------------------------------------------------------------------------------
--this doesn't quite work the way it probably should in all situations, although it's close
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
--sub test13 "b" (Var "a")
