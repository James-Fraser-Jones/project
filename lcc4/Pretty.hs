module Pretty(Show, pError) where

import Types

--{-
instance Show Expr where
  show (App (App e1 e2) e) = (show (App e1 e2)) ++ " " ++ (maybrace e) --application is left associative
  show (App e1 e2) = (maybrace e1) ++ " " ++ (maybrace e2)
  show (Abs a v e1 e2) = (show a) ++ v ++ ":" ++ (maybrace e1) ++ " → " ++ (show e2) --abstraction is right associative
  show (Var v) = v
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

instance Show TypeError where
  show BoxError = "Type Error: Attempted to get the type of Box"
  show LookupError = "Type Error: Attempted to get the type of a free variable"
  show MismatchAppError = "Type Error: Lambda abstraction type does not match the expression it's applied to"
  show NonLamAppError = "Type Error: Non lambda abstraction is applied to an expression"
  show NonSortError = "Type Error: Type of an expression is not a sort where expected"

pError :: Either TypeError Expr -> String
pError (Left a) = show a
pError (Right b) = show b
--------------------------------------------------------------------------------------------------------
braced :: Expr -> Bool
braced (Lit l) = False
braced (Var v) = False
braced _ = True

brace :: String -> String
brace s = "(" ++ s ++ ")"

maybrace :: Expr -> String
maybrace e = (if braced e then brace else id) $ show e
