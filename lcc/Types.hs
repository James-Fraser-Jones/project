module Types where

type Name = String
--data E = Var Name | Star | Box | App E E | Lam Name E E | Dep Name E E deriving Eq
data E = Lit Term | LitT Type | Var Name | Star | Box | App E E | Lam Name E E | Dep Name E E deriving Eq
data Term = B Bool | I Int deriving Eq
data Type = Bool | Int deriving (Eq, Show)

type Context = [(E, E)]
