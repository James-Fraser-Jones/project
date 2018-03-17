module Parser where

import Types

import Prelude hiding (pi, abs)
import Data.Char
import Data.Foldable (foldl')
import Data.Functor
import Control.Applicative hiding (some, many)
import Control.Monad

--heavily inspired by http://dev.stephendiehl.com/fun/002_parsers.html
--and uob language engineering material

--------------------------------------------------------------------------------
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

many :: Parser a -> Parser [a]
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

--------------------------------------------------------------------------------
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

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = scan
  where
    scan = do{ x <- p; rest x }
    rest x = do{ f <- op; y <- scan; return (f x y)}
      <|> return x

token :: Parser a -> Parser a
token px = (many $ satisfy isSpace) *> px

parens :: Parser a -> Parser a
parens px = (char '(') *> px <* (char ')')

--------------------------------------------------------------------------------
--Primitives

box :: Parser String
box = string "☐"
  <|> string "[]"

star :: Parser String
star = string "★"
   <|> string "*"

bool :: Parser Bool
bool = read <$> ((string "True") <|> (string "False"))

lower :: Parser Char
lower = satisfy isLower

digit :: Parser Char
digit = satisfy isDigit

nat :: Parser Int
nat = read <$> some digit

lam :: Parser String
lam = string "λ"
  <|> string "\\"

pi :: Parser String
pi = string "Π"
 <|> string "^"

arr :: Parser String
arr = string "→"
  <|> string "->"

app :: Parser String
app = string "@"

--------------------------------------------------------------------------------
--Expressions

abs :: Parser Abs
abs = lam *> pure Lam
  <|> pi  *> pure Pi

term :: Parser Term
term = B <$> bool
   <|> N <$> nat

type' :: Parser Type
type' = (string "Bool") *> pure Bool
    <|> (string "Nat")  *> pure Nat

sort :: Parser Sort
sort = box *> pure Box
  <|> star *> pure Star

lit :: Parser Lit
lit = Sort <$> sort
  <|> Type <$> type'
  <|> Term <$> term

name :: Parser Name
name = (:) <$> lower <*> (many (lower <|> digit))

exprNoL :: Parser Expr
exprNoL = parens expr
      <|> Abs <$> abs <*> (Just <$> name) <*> (char ':' *> expr) <*> (arr *> expr)
      <|> Abs <$> abs <*> (pure Nothing) <*>              expr  <*> (arr *> expr)
      <|> Var <$> name
      <|> Lit <$> lit

expr :: Parser Expr
expr = chainl1 exprNoL (app *> pure App)

getExpr :: String -> Expr
getExpr s = runParser expr (removeW s)
  where removeW = foldr (\c cs -> if isSpace c then cs else c:cs) []
