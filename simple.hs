import Prelude
import Data.Maybe

----------------------------

data Base = O | I | Nat | Bool deriving (Show, Eq)

data Type = Arr Type Type
          | Base Base
          deriving Eq
instance Show Type where
  show (Base b) = show b
  show (Arr t1 t2) = show t1 ++ " -> " ++ show t2

data Const = U ()
           | N Int
           | B Bool
instance Show Const where
  show (U ()) = show ()
  show (N x) = show x
  show (B b) = show b

c2b :: Const -> Base
c2b (U _) = I
c2b (N _) = Nat
c2b (B _) = Bool

----------------------------

type Name = String

data Expr = Var Name
          | Lam Name Type Expr
          | App Expr Expr
          | Con Const
instance Show Expr where
  show (Con c) = show c
  show (Var x) = x
  show (Lam x ty e) = "\\" ++ x ++ ":" ++ (show ty) ++ "." ++ (show e)
  show (App e1 e2) = (show e1) ++ " " ++ (show e2)

----------------------------

type Tenv = Name -> Maybe Type

empty :: Tenv
empty _ = Nothing --out of scope error

update :: Tenv -> Name -> Type -> Tenv
update t s ty s' = if s' == s then Just ty else t s'

----------------------------

typeCheck :: Tenv -> Expr -> Maybe Type
typeCheck t (Con c) = Just (Base $ c2b c)
typeCheck t (Var x) = t x
typeCheck t (Lam x ty e)
  | typeCheck (update t x ty) e == Nothing = Nothing --recursive error passing
  | otherwise = Just (Arr ty $ fromJust (typeCheck (update t x ty) e))
typeCheck t (App e1 e2)
  | (typeCheck t e1) == Nothing = Nothing --recursive error passing
  | (typeCheck t e2) == Nothing = Nothing --recursive error passing
  | otherwise = case fromJust (typeCheck t e1) of
      (Arr a b) -> if a == fromJust (typeCheck t e2) then Just b else Nothing --incorrect types error
      otherwise -> Nothing --application of non-function error

----------------------------
testa = (Lam "x" (Base Nat) (Con $ B True))
testb = (Con $ N 5)
testc = (Lam "y" (Base Nat) (Var "y"))
testd = (Lam "z" (Arr (Base Nat) (Base Nat)) testa)
test1 = App testa testb
test2 = App (App (testd) (testc)) testb
-- testa :: Nat -> Bool, testd :: (Nat -> Nat) -> (Nat -> Bool)
-- testc :: Nat -> Nat, test b :: Nat
-- test2 :: ((Nat -> Nat) -> (Nat -> Bool)) (Nat -> Nat) Nat
--        = (Nat -> Bool) Nat
--        = Bool
