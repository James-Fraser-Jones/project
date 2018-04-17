module Printer(Show, pEither, pCalc) where
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
  show (Func f) = show f

instance Show Sort where
  show Box = "☐"
  show Star = "★"

instance Show Term where
  show (B b) = show b
  show (N n) = show n

instance Show Func where
  show Plus = "+"
  show And = "&"

instance Show Error where
  show e = case e of
    BoxError ->         "Attempted to get the type of Box"
    LookupError ->      "Variable is not in scope"
    MismatchAppError -> "Lambda function type does not match the expression it's applied to"
    NonLamAppError ->   "Non lambda function is applied to an expression"
    NonSortError ->     "Expression is not well typed"
    RemainError ->      "Parser did not consume entire string"
    GeneralError ->     "Invalid input string"
--------------------------------------------------------------------------------------------------------
--Other printing functions

pCalc :: Calculus -> String
pCalc c = case c of
    S ->    "Simply Typed Lambda Calculus, λ→"
    SP ->   "Second Order Lambda Calculus, System F, λ2"
    ST ->   "Weak λω"
    SD ->   "Logical Framework, λP"
    SPT ->  "System F Omega, λω"
    STD ->  "Weak λPω"
    SPD ->  "λP2"
    SPTD -> "Calculus of Constructions, λPω"

pEither :: (Show a, Show b) => Either a b -> String
pEither (Left a) = show a
pEither (Right b) = show b
