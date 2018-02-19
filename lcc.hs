
type Name = String

data E = Var Name | Star | Box | App E E | Lam Name E E | Dep Name E E deriving Eq

instance Show E where
  --app rules
  show (App (App e1 e2) x) = (show (App e1 e2)) ++ " " ++ (maybrace x)
  show (App x y) = (maybrace x) ++ " " ++ (maybrace y)
  --lam rule
  show (Lam s e1 e2) = "λ" ++ s ++ ":" ++ (maybrace e1) ++ "." ++ (show e2)
  --dep rules (with sugar)
  show (Dep s e1 e2)
    -- | (Dep s e1 e2) == (Dep s Star Star) = (show e1) ++ "→" ++ (show e2) --this is incorrect because it's the TYPE of e1 and e2 which need to be * not themselves
    | otherwise = "∏" ++ s ++ ":" ++ (maybrace e1) ++ "." ++ (show e2)
  --non recursive definitions
  show (Var s) = s
  show Star = "★"
  show Box = "☐"

brace :: String -> String
brace s = "(" ++ s ++ ")"

braced :: E -> Bool
braced (Var s) = False
braced Star = False
braced Box = False
braced _ = True

maybrace :: E -> String
maybrace x = if braced x then brace (show x) else show x

termToTerm :: E --the TYPE of a function from terms to terms
termToTerm = Dep "_" Star Star

typeToType :: E --the TYPE of a function from types to types
typeToType = Dep "_" Box Box
