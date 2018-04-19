import Data.List (union, span, (\\))
import Control.Monad.Reader

type Name = String

data Exp
  = Var Name
  | App Exp Exp
  | Lam Name Exp
  deriving (Eq,Show,Read)

---------------------------------------------------------
ex1 :: Exp
ex1 = (Lam "x" (App (Var "x") (Var "y")))

vars :: Exp -> Reader Bool [Name]
vars (Var x) = return [x]
vars (App a b) = do
  a' <- vars a
  b' <- vars b
  return $ union a' b'
vars (Lam n x) = do
  isFree <- ask
  let f = if isFree then (\\ [n]) else id
  x' <- vars x
  return $ f x'

freeVars :: Exp -> [Name]
freeVars e = runReader (vars e) True

allVars :: Exp -> [Name]
allVars e = runReader (vars e) False
---------------------------------------------------------

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
