module Pretty(Show, pEither) where
import Types
--------------------------------------------------------------------------------------------------------
--Utility functions

braced (Lit _) = False
braced (Var _) = False
braced _ = True
maybrace e = (if braced e then brace else id) $ show e
  where brace s = "(" ++ s ++ ")"
--------------------------------------------------------------------------------------------------------
--Show instances

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

instance Show TypeError where
  show BoxError = "Type Error: Attempted to get the type of Box"
  show LookupError = "Type Error: Attempted to get the type of a free variable"
  show MismatchAppError = "Type Error: Lambda function type does not match the expression it's applied to"
  show NonLamAppError = "Type Error: Non lambda function is applied to an expression"
  show NonSortError = "Type Error: Expression is not well typed"

instance Show ParseError where
  show RemainError = "Parser did not consume entire stream"
  show GeneralError = "Parser error"
--------------------------------------------------------------------------------------------------------
--Pretty Either printing

pEither :: (Show a, Show b) => Either a b -> String
pEither (Left a) = show a
pEither (Right b) = show b
