module Types where

import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Functor

type Name = String
type Context = [(Name, Expr)] --the expr represented by each var has type expr

data Expr = Lit Literal | Var Name | App Expr Expr | Lam Name Expr Expr | Pi Name Expr Expr deriving Eq
data Literal = Top | Kind | Type TypeLit | Term TermLit deriving Eq
data TypeLit = Bool | Nat deriving (Eq, Show)
data TermLit = B Bool | N Int deriving Eq

starBox :: Maybe Expr -> Bool
starBox (Just (Lit Top)) = True
starBox (Just (Lit Kind)) = True
starBox _ = False

add :: Name -> Expr -> Context -> Context
add "_" _ c = c --don't add empty named variables to the context
add  s  e c = (s,e):c

litType :: Literal -> Maybe Expr
litType (Term t) =
  case t of
    (B _) -> Just $ Lit (Type Bool)
    (N _) -> Just $ Lit (Type Nat)
litType (Type _) = Just $ Lit Kind
litType    Kind  = Just $ Lit Top
litType     Top  = Nothing

getType :: Context -> Expr -> Maybe Expr
getType _ (Lit l) = litType l

getType c (Var s) =
  case s of
    "_" -> Nothing    --nameless variable for use with normal and term types
    _   -> lookup s c --lookup behaves correctly with "add" function

--getType c (App p q) = if then (sub a x b') else Nothing
--  where (Pi x a r) = (getType c p)

getType c (Lam x a b) = if and[isJust b', starBox t] then Just pi else Nothing
  where b' = (getType (add x a c) b)
        pi = (Pi x a (fromJust b')) --this doesn't crash when b' is nothing because of the "and" condition above
        t = (getType c pi)

getType c (Pi x a b) = if and[starBox s, starBox t] then t else Nothing
  where s = (getType c a)
        t = (getType (add x a c) b)

-------------------------------------------------------------------------------------------------------------------
--This is the section where we define substitution and beta reduction

fvars :: Expr -> Set Name --get a set containing every instance of a free variable in the expression
fvars (Lit l) = Set.empty
fvars (Var "_") = Set.empty
fvars (Var a) = Set.singleton a
fvars (App p q) = Set.union (fvars p) (fvars q)
fvars (Lam x t p) = Set.delete x (fvars p)
fvars (Pi  x t p) = Set.delete x (fvars p)

vars :: [Name]
vars = (\(a, b) -> ['a'..'z'] !! b : if a == 0 then "" else show $ a+1).(flip quotRem 26) <$> [0..]

fresh :: Set Name -> Name --given a set of used variable names, return a fresh variable from an infinite list
fresh s = head $ filter (flip Set.notMember s) vars

sub :: Expr -> Name -> Expr -> Expr
sub n x p = sub' (fvars n) n x p

--In order: Free variables of N, N (the expression to be subbed in), x (the variable to sub out), E (the expression being subbed into)
sub' :: Set Name -> Expr -> Name -> Expr -> Expr
sub' fvn n x (Lit l) = Lit l
sub' fvn n x (Var a) = if x == a then n else Var a
sub' fvn n x (App p q) = App (sub' fvn n x p) (sub' fvn n x q)

sub' fvn n x (Lam y t p) =
  if                x == y then Lam y t p                else
    if Set.notMember y fvn then Lam y t (sub' fvn n x p) else
                                Lam z t (sub' fvn n x p')
                                where p' = (sub (Var z) y p)
                                      z = fresh (Set.insert x (Set.union fvn fvp))
                                      fvp = fvars p

sub' fvn n x (Pi y t p) =
  if                x == y then Pi y t p                else
    if Set.notMember y fvn then Pi y t (sub' fvn n x p) else
                                Pi z t (sub' fvn n x p')
                                where p' = (sub (Var z) y p)
                                      z = fresh (Set.insert x (Set.union fvn fvp))
                                      fvp = fvars p
