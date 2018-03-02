module Types where

import Data.Maybe

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

--getType for app requires substitution to be defined first

getType c (Lam x a b) = if and[isJust b', starBox t] then Just pi else Nothing
  where b' = (getType (add x a c) b)
        pi = (Pi x a (fromJust b')) --this doesn't crash when b' is nothing because of the "and" condition above
        t = (getType c pi)

getType c (Pi x a b) = if and[starBox s, starBox t] then t else Nothing
  where s = (getType c a)
        t = (getType (add x a c) b)
