module Types where

import Data.Maybe

type Name = String
type Index = Int
type Var = (Name, Index)
type IndexCon = [Var]

data Expr = Lit Literal | Var Var | App Expr Expr | Lam (Maybe Name) Expr Expr | Pi (Maybe Name) Expr Expr deriving Eq
data Literal = Top | Kind | Type TypeLit | Term TermLit deriving Eq
data TypeLit = Bool | Nat deriving (Eq, Show)
data TermLit = B Bool | N Int deriving Eq

shift :: Index -> Int -> Expr -> Expr
shift k n (Var (s, i)) = Var (s, i + (if i > k then n else 0))
shift k n (Lam s e1 e2) = Lam s (shift (k+1) n e1) (shift (k+1) n e2) --is it actually correct to also recurse down e1 here?
shift k n (App e1 e2) = App (shift k n e1) (shift k n e2)
shift _ _ (Lit l) = Lit l
--do I add a rule for pi? does it make sense to do beta reduction with pis?

--all free variables are shifted
shiftFree = shift 0

sub :: Index -> Expr -> Expr -> Expr
sub k e (Var (s, i))
  |  i < k = Var (s, i)
  | i == k = shiftFree (k-1) e
  |  i > k = Var (s, i-1)
sub k e (Lam s e1 e2) = Lam s (sub (k+1) e e1) (sub (k+1) e e2)
sub k e (App e1 e2) = App (sub k e e1) (sub k e e2)
sub _ _ (Lit l) = Lit l

beta :: Expr -> Expr
beta (App (Lam (Just s) _ e2) e) = sub 1 e e2
beta (App (Lam  Nothing _ e2) _) = e2
beta (App e1 e2) = App (beta e1) (beta e2)
beta (Lam s e1 e2) = Lam s (beta e1) (beta e2)
beta e = e

index :: IndexCon -> Index -> Expr -> Expr
index c i (Var (s,_)) =
  case lookup s c of
    Nothing  -> Var (s,i+1) --variable is free
    (Just n) -> Var (s,i-n) --variable is bound
index c i (Lam (Just s) e1 e2) = Lam (Just s) (index ((s,i):c) (i+1) e1) (index ((s,i):c) (i+1) e2)
index c i (Lam  Nothing e1 e2) = Lam  Nothing (index        c  (i+1) e1) (index        c  (i+1) e2)
index c i (App e1 e2) = App (index c i e1) (index c i e2)
index _ _ (Lit l) = Lit l

sindex = index [] 0
