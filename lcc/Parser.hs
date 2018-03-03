module Parser where

import Types

import Prelude hiding (pi)

import Data.Char
import Data.Foldable (foldl')
import Data.Functor

import Control.Applicative hiding (some, many)
import Control.Monad

--heavily inspired by http://dev.stephendiehl.com/fun/002_parsers.html
--and uob language engineering material

--------------------------------------------------------------------------------
--Basic parsers and parser combinators

removeW :: String -> String
removeW = foldr (\c cs -> if isSpace c then cs else c:cs) []

newtype Parser a = Parser { parse :: String -> [(a, String)] }

runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(res, [])] -> res
    [(_, rs)]   -> error "Parser did not consume entire stream."
    _           -> error "Parser error."

runParserW :: Parser a -> String -> a
runParserW m s = runParser m (removeW s)

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
  <|> string "Λ"
  <|> string "\\"

pi :: Parser String
pi = string "Π"
 <|> string "^"

forA :: Parser String
forA = string "∀"
  <|> string "\\/"

arr :: Parser String
arr = string "→"
  <|> string "->"

app :: Parser String
app = string "@"

--------------------------------------------------------------------------------
--Expressions

termLit :: Parser TermLit
termLit = B <$> bool
      <|> N <$> nat

typeLit :: Parser TypeLit
typeLit = (string "Bool") *> pure Bool
      <|> (string "Nat")  *> pure Nat

lit :: Parser Literal
lit = box  *> pure Top
  <|> star *> pure Kind
  <|> Type <$> typeLit
  <|> Term <$> termLit

var :: Parser Name
var = (:) <$> lower <*> (many (lower <|> digit))
  <|> (string "_")

exprNoL :: Parser Expr
exprNoL = parens expr
      <|> Lam <$> (lam *> var) <*> (char ':' *> expr) <*> (char '.' *> expr)
      <|> Pi  <$> ((pi <|> forA) *> var) <*> (char ':' *> expr) <*> (char '.' *> expr)
      <|> Var <$> var
      <|> Lit <$> lit

exprApp :: Parser Expr
exprApp = chainl1 exprNoL (app *> pure App)

expr :: Parser Expr --arrow operator has the higher prescedence than application or any other operator
expr = chainr1 exprApp (arr *> pure arrow)
  where arrow e1 e2 = Pi "_" e1 e2

--------------------------------------------------------------------------------
--Examples

polyId = "\\a:*.\\x:a.x" --polymorphic identity function for terms of type a
polyIdType = "^a:*.^_:a.a" --type of polymorphic identity function for terms of type a

fmapType = "^f:(^_:*.*).^a:*.^b:*.(^_:(^_:a.b).(^_:f @ a.f @ b))" --fmap :: (a -> b) -> f a -> f b
fmapTypeB = "^f:^_:*.*.^a:*.^b:*.^_:^_:a.b.^_:f @ a.f @ b" --fmap definition without brackets parsed the same way

fmapTypeC = "∀f:* -> *.∀a:*.∀b:*.(a -> b) -> f @ a -> f @ b" --since arrow operator has highest prescedence, brackets are
--not necessary around the applications f a and f b

listType = "^_:*.*"

getExpr' = runParserW expr
getType' = (getType []).getExpr'

{-
fmap :: ∀f:    (* -> *) . (∀a:* .(∀b:*. (   (a -> b) -> (    f a -> f b))))
        Πf:(Πx:★  . ★) . (Πa:★.(Πb:★.(Πx:(Πx:a.b)  . (Πx:(f a) . f b))))

application is left associative but type constrction is right associative, not certain that this is correct in my parser
right associative type contruction only matters when you're parsing (a -> b -> c) otherwise this would be:
 ^_:a.^_:b.c which is not ambiguous since the left associative version would look like this: ^_:^_:a.b.c

application interacts with pi in exactly the same way that it interacts with lam
the reason that (* -> *) becomes Π_:*.*) is because there aren't any variables to be replaced with an expression in the second
input (namely, *) hence, we don't need to use any particular variable name since we know it can never be used during application
the type of a polymorphic function, for instance can become the type of a non-polymophic instance of that function when you
feed it a concrete type as input, in this way application is used and the Pi behaves as a function (like lam)

parse expr "\\a:*.\\x:a.x"
runParser expr "x@(1@3)@\\a:x@b.\\x:a.x"
runParserW expr "x @ (hey @ *) @ \\a:x @ Box.\\x:a.x"
-}
