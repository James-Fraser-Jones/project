module Parser where

import Types

import Data.Char
import Data.Foldable (foldl')
import Data.Functor

import Control.Applicative hiding (some, many)
import Control.Monad

--heavily inspired by http://dev.stephendiehl.com/fun/002_parsers.html

------------------------------------------------
--Basic parsers and parser combinators

newtype Parser a = Parser { parse :: String -> [(a, String)] }

runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(res, [])] -> res
    [(_, rs)]   -> error "Parser did not consume entire stream."
    _           -> error "Parser error."

instance Functor Parser where
 fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s]) --I've kinda forgotten how list comprehensions work

instance Applicative Parser where
 pure a = Parser (\s -> [(a,s)]) --produce
 (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

instance Monad Parser where
 return = pure
 p >>= f = Parser (\s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s)

instance Alternative Parser where
 empty = Parser (\cs -> []) --failure
 p <|> q = Parser (\s -> case parse p s of
   []  -> parse q s
   res -> res)

some :: Parser a -> Parser [a]
some v = some_v
  where
    many_v = some_v <|> pure []
    some_v = (:) <$> v <*> many_v

many :: Parser a -> Parser [a] --
many v = many_v
  where
    many_v = some_v <|> pure []
    some_v = (:) <$> v <*> many_v

item :: Parser Char
item = Parser (\s -> case s of
  []     -> []
  (c:cs) -> [(c,cs)])

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>= \c ->
  if p c
    then pure c
    else empty

------------------------------------------------
--Higher order parsers

char :: Char -> Parser Char
char c = satisfy (c ==)

string :: String -> Parser String
string []     = pure []
string (c:cs) = char c >>= \c' ->
                string cs >>= \cs' ->
                pure (c:cs)

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (flip elem s)

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a --this was taken from Jamie
chainl1 p op = foldl' (flip ($)) <$> p <*> many (flip <$> op <*> p)

token :: Parser a -> Parser a
token px = spaces *> px

parens :: Parser a -> Parser a
parens px = (char '(') *> px <* (char ')')

------------------------------------------------
--Primitives

boolean :: Parser Bool
boolean = read <$> ((string "True") <|> (string "False"))

lower :: Parser Char
lower = satisfy isLower

digit :: Parser Char
digit = satisfy isDigit

natural :: Parser Int
natural = read <$> some digit

spaces :: Parser String
spaces = many (satisfy isSpace)

lam :: Parser String
lam = string "λ"
  <|> string "\\"
  <|> string "Lam"

dep :: Parser String
dep = string "Π"
  <|> string "^"
  <|> string "Dep"

star :: Parser String
star = string "★"
   <|> string "*"
   <|> string "Star"

box :: Parser String
box = string "☐"
  <|> string "[]"
  <|> string "Box"

------------------------------------------------
--Expression parsers

termLit :: Parser TermLit
termLit = B <$> boolean
      <|> N <$> natural

typeLit :: Parser TypeLit
typeLit = (string "Bool") *> pure Bool
      <|> (string "Nat")  *> pure Nat

literal :: Parser Literal
literal = box  *> pure Top
      <|> star *> pure Kind
      <|> Type <$> typeLit
      <|> Term <$> termLit

name :: Parser Name
name = (:) <$> lower <*> (many (lower <|> digit))

app :: Parser (Expr -> Expr -> Expr)
app = char '@' *> pure App

exprNoApp :: Parser Expr
exprNoApp = parens expr
        <|> Lam <$> (lam *> name) <*> (char ':' *> expr) <*> (char '.' *> expr)
        <|> Dep <$> (dep *> name) <*> (char ':' *> expr) <*> (char '.' *> expr)
        <|> Var <$> name
        <|> Lit <$> literal

expr :: Parser Expr
expr = chainl1 exprNoApp app

-- parse expr "\\a:*.\\x:a.x"
-- runParser expr "x@(1@3)@\\a:x@b.\\x:a.x"
