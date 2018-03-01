module Types where

type Name = String

data Expr = Lit Literal | Var Name | App Expr Expr | Lam Name Expr Expr | Dep Name Expr Expr deriving Eq

data Literal = Top | Kind | Type TypeLit | Term TermLit deriving Eq

data TypeLit = Bool | Nat deriving (Eq, Show)

data TermLit = B Bool | N Int deriving Eq

type Context = [(Expr, Expr)]
