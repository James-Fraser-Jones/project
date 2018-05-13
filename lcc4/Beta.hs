module Beta (alpha, beta, delta, normalize, typeCheck) where
import Types

import Data.List
import Data.Maybe
import Data.Functor
import Control.Monad.Reader
--------------------------------------------------------------------------------------------------------
--Alpha equivalence

canonym' :: Cantext -> String -> Expr -> Expr
canonym' _ _ (Lit l) = Lit l
canonym' c _ (Var x) = Var $ if isJust l then fromJust l else "F" --all (globally) free variables are alpha equivalent
  where l = lookup x c
canonym' c s (App e1 e2) = App (canonym' c ('L':s) e1) (canonym' c ('R':s) e2)
canonym' c s (Abs a v e e') = Abs a new (canonym' c s e) (canonym' ((v,new):c) new e')
  where new = ('N':s)

canonym :: Expr -> Expr
canonym = canonym' [] "P" --only lowercase variable names can be parsed so "P" can't accidentally capture a variable name

alpha :: Expr -> Expr -> Bool
alpha e1 e2 = (canonym e1) == (canonym e2)
--------------------------------------------------------------------------------------------------------
--Beta reduction

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

newId :: [Var] -> Var
newId vs = head (names \\ vs)

names :: [Var]
names = [ [i] | i <- ['a'..'z']] ++ [i : show j | j <- [1..], i <- ['a'..'z'] ]

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

beta :: Expr -> Expr
beta (App (Abs _ v _ e2) e) = subst v e e2 --reduction
beta (App     e1 e2) = App     (beta e1) (beta e2) --propagation
beta (Abs a v e1 e2) = Abs a v (beta e1) (beta e2)
beta e = e --neither reduction nor propagation

normalize :: Expr -> Expr
normalize e = if e == e' then e' else normalize e'
  where e' = beta e
--------------------------------------------------------------------------------------------------------
--Typechecking

getSort :: Expr -> Either Error Expr
getSort (Lit (Sort s)) = Right $ Lit (Sort s)
getSort _ = Left NonSortError

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
  getSort p'
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
appComp (Abs Pi x t1 b) e t2 = if alpha t1 t2 then Right $ subst x e b else Left MismatchAppError
appComp _ _ _ = Left NonLamAppError

typeCheck :: AbsForms -> Expr -> Either Error Expr
typeCheck = tC [] --begin typechecking with empty context
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
