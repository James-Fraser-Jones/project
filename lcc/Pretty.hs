module Pretty(Show) where

import Types

instance Show E where
  --app rules
  show (App (App e1 e2) x) = (show (App e1 e2)) ++ " " ++ (maybrace x)
  show (App x y) = (maybrace x) ++ " " ++ (maybrace y)
  --lam rule
  show (Lam s e1 e2) = "λ" ++ s ++ ":" ++ (maybrace e1) ++ "." ++ (show e2)
  --dep rule
  show (Dep s e1 e2) = "Π" ++ s ++ ":" ++ (maybrace e1) ++ "." ++ (show e2)
  --variables
  show (Var s) = s
  --literals
  show Box = "☐"
  show Star = "★"
  show (LitT ty) = show ty
  show (Lit t) = show t

instance Show Term where
  show (B b) = show b
  show (I i) = show i

braced :: E -> Bool
braced (Var s) = False
braced Star = False
braced Box = False
braced _ = True

brace :: String -> String
brace s = "(" ++ s ++ ")"

maybrace :: E -> String
maybrace x = if braced x then brace (show x) else show x
