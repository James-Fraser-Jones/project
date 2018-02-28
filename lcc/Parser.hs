module Parser where
import Types
import Control.Applicative
import Data.Char
------------------------------------------------
--General parsers

newtype Parser a = Parser (String -> [(String, a)])

produce :: a -> Parser a
produce x = Parser (\ts -> [(ts, x)])

parse :: Parser a -> (String -> [(String, a)])
parse (Parser p) = p

failure :: Parser a
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

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a} --this was taken from http://dev.stephendiehl.com/fun/002_parsers.html
 where rest a = (do f <- op
                    b <- p
                    rest (f a b))
                <|> return a

{-
--Black magicks below

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = foldl' (flip ($)) <$> p <*> many (flip <$> op <*> p)

op :: Parser (a -> b -> c)
p :: Parser b
flip <$> op :: Parser (b -> a -> c)
flip <$> op <*> p :: Parser (a -> c)
many (flip <$> op <*> p) :: Parser [a -> a]
flip ($) :: a -> (a -> a) -> a
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' (flip ($)) <$> :: Parser a -> Parser [a -> a] -> Parser a

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = flip (foldr ($)) <$> many (try (p) <**> op) <*> p

char ('\\') $> Lambda

(<**>) :: Parser a -> Parser (a -> b) -> Parser b
(<**>) = liftA2 (flip ($))
--}

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

w :: Parser a -> Parser a --remove whitespace from beginning of input
w (Parser px) = Parser (px.spaces)
  where spaces (' ':xs) = spaces xs
        spaces s = s

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
expr = Lam <$> (char '\\' *> name) <*> (char ':' *> expr) <*> (char '.' *> expr)
   <|> Dep <$> (char  '^' *> name) <*> (char ':' *> expr) <*> (char '.' *> expr)
   <|> Var <$> name
   <|> Lit <$> term
   <|> LitT <$> type'
   <|> string "[]" *> pure Box
   <|> char '*' *> pure Star
   <|> (char '(') *> expr <* (char ')')
   <|> chainl1 expr app

app :: Parser (E -> E -> E)
app = char '@' *> pure App

-- parse expr "\\a:*.\\x:a.x"
