module Parser where
import Prelude hiding (concat)
import Types
import Control.Applicative
import Data.Char
import Data.Foldable
------------------------------------------------
--General parsers

newtype Parser a = Parser (String -> [(String, a)])

produce :: a -> Parser a
produce x = Parser (\ts -> [(ts, x)])

parse :: Parser a -> (String -> [(String, a)]) --this can be included in the Parser a definition
parse (Parser p) = p

failure :: Parser a --surely this is just produce [] which itself is pure [] ?
failure = Parser (\ts -> [])

item :: Parser Char
item = Parser (\ts -> case ts of
  [] -> []
  (x:xs) -> [(xs, x)])

instance Functor Parser where
  fmap f (Parser px) = Parser(\ts -> [(ts', f x) | (ts', x) <- px ts])

instance Applicative Parser where
  pure = produce
  Parser pf <*> Parser px = Parser (\ts -> [ (ts'', f x )| (ts', f) <- pf ts,
                                                           (ts'', x) <- px ts'])

instance Monad Parser where
  return = produce
  Parser px >>= f = Parser (\ts ->
    concat([parse (f x) ts' | (ts', x) <- px ts]))

instance Alternative Parser where
  empty = failure
  (<|>) = orElse

orElse :: Parser a -> Parser a -> Parser a
orElse (Parser px) (Parser py) = Parser (\ts ->
  case px ts of
        [] -> py ts
        xs -> xs )

some' :: Parser a -> Parser [a] --this was taken from http://dev.stephendiehl.com/fun/002_parsers.html
some' v = some_v
  where
    many_v = some_v <|> pure [] --unsure why this can't just be failure but it seems to break everything
    some_v = (:) <$> v <*> many_v

many' :: Parser a -> Parser [a] --this was taken from http://dev.stephendiehl.com/fun/002_parsers.html
many' v = many_v
  where
    many_v = some_v <|> pure []
    some_v = (:) <$> v <*> many_v

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a --this was taken from Jamie
chainl1 p op = foldl' (flip ($)) <$> p <*> many' (flip <$> op <*> p)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>= \c ->
                if p c
                   then produce c
                   else failure

char :: Char -> Parser Char
char c = satisfy (c==)

string :: String -> Parser String
string []     = produce []
string (c:cs) = char c >>= \c' ->
                string cs >>= \cs' ->
                produce (c:cs)

------------------------------------------------
--More specific parsers

digit :: Parser Char
digit = satisfy isDigit

lower :: Parser Char
lower = satisfy isLower

lowerDigit :: Parser Char
lowerDigit = lower
         <|> digit

int :: Parser Int
int = read <$> (some' digit)

whitespace :: Parser String
whitespace = many' (satisfy isSpace)

token :: Parser a -> Parser a
token px = whitespace *> px

------------------------------------------------
--Expression parsers

name :: Parser Name --lowercase letters or numbers, no number in first char
name = (:) <$> lower <*> (many' lowerDigit)

term :: Parser Term
term = string "True" *> pure (B True)
   <|> string "False" *> pure (B False)
   <|> I <$> int

type' :: Parser Type
type' = string "Bool" *> pure Bool
    <|> string "Int" *> pure Int

expr :: Parser E
expr = chainl1 exprNoApp app

exprNoApp :: Parser E
exprNoApp = Lam <$> (char '\\' *> name) <*> (char ':' *> expr) <*> (char '.' *> expr)
        <|> Dep <$> (char  '^' *> name) <*> (char ':' *> expr) <*> (char '.' *> expr)
        <|> Var <$> name
        <|> Lit <$> term
        <|> LitT <$> type'
        <|> string "[]" *> pure Box
        <|> char '*' *> pure Star
        <|> (char '(') *> expr <* (char ')')

app :: Parser (E -> E -> E)
app = char '@' *> pure App

-- parse expr "\\a:*.\\x:a.x"
