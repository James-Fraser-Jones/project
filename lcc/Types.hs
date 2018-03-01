module Types where

type Name = String
type Context = [(Name, Expr)] --each the expr represented by each var has type expr

data Expr = Lit Literal | Var Name | App Expr Expr | Lam Name Expr Expr | Dep Name Expr Expr deriving Eq
data Literal = Top | Kind | Type TypeLit | Term TermLit deriving Eq
data TypeLit = Bool | Nat deriving (Eq, Show)
data TermLit = B Bool | N Int deriving Eq

litType :: Literal -> Maybe Expr
litType (Term t) = case t of
  (B _) -> Just $ Lit (Type Bool)
  (N _) -> Just $ Lit (Type Nat)
litType (Type _) = Just $ Lit Kind
litType    Kind  = Just $ Lit Top
litType     Top  = Nothing

getType :: Context -> Expr -> Maybe Expr
getType _ (Lit l) = litType l
getType c (Var s) = case s of
  "_" -> Nothing --nameless variable for use with normal and term types
  _ -> lookup s c --you need to ensure that lookup correctly chooses the most recent binding of a variable from the list

--app does not (and should not) interact with deps, the only purpose of dep is to represent the type of a lam

{-
getType (App e1 e2) = case e1 of
  (Lam e1' e2') = if and[((getType e1') == (getType e2)), ((getType e1') != Nothing)] then Just e2' else Nothing
      otherwise = Nothing
getType (Lam s e1 e2) =

getType (Dep s e1 e2) = case (getType e2) of
-}
