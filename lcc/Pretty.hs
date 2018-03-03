module Pretty(Show) where

import Types

instance Show Expr where
  --app rules
  show (App (App e1 e2) x) = (show (App e1 e2)) ++ " " ++ (maybrace x) --application is left associative
  show (App x y) = (maybrace x) ++ " " ++ (maybrace y)
  --lam rule
  show (Lam s e1 e2) = "λ" ++ s ++ ":" ++ (maybrace e1) ++ "." ++ (show e2) --still unsure whether to brace expr after colon
  --pi rules
  show (Pi s e1 e2) = "Π" ++ s ++ ":" ++ (maybrace e1) ++ "." ++ (show e2)
  --variables
  show (Var s) = s
  --literals
  show (Lit l) = show l

instance Show Literal where
  show Top = "☐ "
  show Kind = "★ "
  show (Type ty) = show ty
  show (Term t) = show t

instance Show TermLit where
  show (B b) = show b
  show (N n) = show n

braced :: Expr -> Bool
braced (Lit l) = False
braced (Var s) = False
braced _ = True

brace :: String -> String
brace s = "(" ++ s ++ ")"

maybrace :: Expr -> String
maybrace x = if braced x then brace (show x) else show x

{-
in order to allow show to correctly display the 4 different Pi types without having to recalculate
the type of the expressions inside pi, I will have to have the following design:

All expressions now have a "type" tag that stores the type of that expression.
When an expressions is parsed in, all expressions and sub expressions are given a
default "no-type" expression as their type.

After this, the expression is fed to a function which will recursively call itself throughout the structure,
assigning types where they are obvious (literals and variables) and building up bigger and bigger types until
the entire expression's type tag is correctly assigned.

From here, whenever show is called on an expression, show need only look at the expression stored in the type
tag to determine how to show it.

A possibility here is that we actually only have the type tags for "Pi" exclusively since its type is the only
one that will be referenced in the show function. I will have to consider this. It somehow seems neater and
possibly more useful to have the type of every expression.

I definiely think you should do it for every expression at the moment.

Also, I can't do a quick fix with this and call "getType" during every "show" call because I also need the relevent Context
which I don't have when show is called in the recursive way it is since no data is passed to it from the previous show function
about the context.
-}
