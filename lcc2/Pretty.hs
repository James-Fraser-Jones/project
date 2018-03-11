module Pretty(Show) where

import Types

--{-
instance Show Expr where
  --app rules
  show (App (App e1 e2) x) = (show (App e1 e2)) ++ " " ++ (maybrace x) --application is left associative
  show (App x y) = (maybrace x) ++ " " ++ (maybrace y)
  --lam rule
  show (Lam (Just n) e1 e2) = "λ" ++ n ++ ":" ++ (maybrace e1) ++ " → " ++ (show e2)
  show (Lam Nothing  e1 e2) = "λ" ++             (maybrace e1) ++ " → " ++ (show e2)
  --pi rules
  show ( Pi (Just n) e1 e2) = "Π" ++ n ++ ":" ++ (maybrace e1) ++ " → " ++ (show e2)
  show ( Pi Nothing  e1 e2) = "Π"             ++ (maybrace e1) ++ " → " ++ (show e2)
  --variables
  show (Var (n, i)) = n
  --literals
  show (Lit l) = show l

instance Show Literal where
  show Top = "☐"
  show Kind = "★"
  show (Type ty) = show ty
  show (Term t) = show t

instance Show TermLit where
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
