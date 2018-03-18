module Pretty(Show) where

import Types

--{-
instance Show Expr where
  --app rules
  show (App (App e1 e2) x) = (show (App e1 e2)) ++ " " ++ (maybrace x) --application is left associative
  show (App x y) = (maybrace x) ++ " " ++ (maybrace y)
  --lam rule
  show (Abs a (Just n) e1 e2) = (show a) ++ n ++ ":" ++ (maybrace e1) ++ " → " ++ (show e2)
  show (Abs a Nothing  e1 e2) = (show a) ++             (maybrace e1) ++ " → " ++ (show e2)
  --variables
  show (Var n) = n
  --literals
  show (Lit l) = show l

instance Show Abs where
  show Lam = "λ"
  show Pi = "Π"

instance Show Lit where
  show (Sort s) = show s
  show (Type ty) = show ty
  show (Term t) = show t

instance Show Sort where
  show Box = "☐"
  show Star = "★"

instance Show Term where
  show (B b) = show b
  show (N n) = show n
--}

braced :: Expr -> Bool
braced (Lit l) = False
braced (Var s) = False
braced _ = True

brace :: String -> String
brace s = "(" ++ s ++ ")"

maybrace :: Expr -> String
maybrace x = if braced x then brace (show x) else show x

instance Show TypeError where
  show BoxError = "Error: Attempted to get type of Sort ☐"
  show LookupError = "Error: Attempted to get type of a free variable"
  show MismatchAppError = "Error: Function is applied to an expression of an incorrect type"
  show NonAbsAppError = "Error: Non-Function is applied to an expression"
  show NonWellFormedError = "Error: Type was not well formed"
