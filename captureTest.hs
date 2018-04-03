import Data.List (union, span, (\\))

type Name = String

data Exp
  = Var Name
  | App Exp Exp
  | Lam Name Exp
  deriving (Eq,Show,Read)

-- show
freeVars :: Exp -> [Name]
freeVars (Var x) = [x]
freeVars (App a b) = freeVars a `union` freeVars b
freeVars (Lam n x) = freeVars x \\ [n]

allVars :: Exp -> [Name]
allVars (Var x) = [x]
allVars (App a b) = allVars a `union` allVars b
allVars (Lam n x) = allVars x

subst :: Name -> Exp -> Exp -> Exp
subst x s b = sub vs0 b where
  sub _ e@(Var v)
    | v == x = s
    | otherwise = e
  sub vs e@(Lam v e')
    | v == x = e
    | v `elem` fvs = Lam v' (sub (v':vs) e'')
    | otherwise = Lam v (sub vs e') where
    v' = newId vs
    e'' = subst v (Var v') e'
  sub vs (App f a) = sub vs f `App` sub vs a
  fvs = freeVars s
  vs0 = fvs `union` allVars b

newId :: [Name] -> Name
newId vs = head (names \\ vs)

names :: [Name]
names = [ [i] | i <- ['a'..'z']] ++ [i : show j | j <- [1..], i <- ['a'..'z'] ]
-- /show

-- show and we can see that this deals with capture avoidance
main = print $ subst "z" (Lam "x" (Var "y")) (Lam "y" (Var "z"))
-- /show
