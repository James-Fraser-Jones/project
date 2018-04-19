import Data.List (union, span, (\\))
import Control.Monad.Reader

type Var = String

data Expr = Lit Lit
          | Var Var
          | App Expr Expr
          | Abs Abs Var Expr Expr deriving Eq

data  Abs = Lam | Pi deriving Eq --used to differentiate functions from function types

data  Lit = Sort Sort | Type Type | Term Term | Func Func deriving Eq

data Sort = Box | Star deriving Eq --the 2 sorts used in the lambda cube language
data Type = Bool | Nat deriving (Eq, Show) --the types of the terms below
data Term = B Bool | N Int deriving Eq --the example literal terms of this language
data Func = Plus | And deriving Eq --example literal functions

---------------------------------------------------------
vars :: Expr -> Reader Bool [Var]
vars (Lit l) = return []
vars (Var x) = return [x]
vars (App a b) = do
  a' <- vars a
  b' <- vars b
  return $ union a' b'
vars (Abs _ v t e) = do
  isFree <- ask
  let f = if isFree then (\\ [v]) else id
  t' <- vars t
  e' <- vars e
  return $ t' `union` (f e')

freeVars :: Expr -> [Var]
freeVars e = runReader (vars e) True

allVars :: Expr -> [Var]
allVars e = runReader (vars e) False
---------------------------------------------------------
newId :: [Var] -> Var
newId vs = head (names \\ vs)

names :: [Var]
names = [ [i] | i <- ['a'..'z']] ++ [i : show j | j <- [1..], i <- ['a'..'z'] ]
---------------------------------------------------------
subst :: Var -> Expr -> Expr -> Expr
subst x s b = sub vs0 b where
  sub _ e@(Lit l) = e --Lit rule
  sub _ e@(Var v) --Var rule
    | v == x = s
    | otherwise = e
  sub vs e@(Abs a v t e') --Lam rule
    | v == x = Abs a v (sub vs t) e'
    | v `elem` fvs = Abs a v' (sub vs t) (sub (v':vs) e'')
    | otherwise = Abs a v (sub vs t) (sub vs e') where
    v' = newId vs
    e'' = subst v (Var v') e'
  sub vs (App f a) = sub vs f `App` sub vs a --App rule
  fvs = freeVars s
  vs0 = fvs `union` allVars b
