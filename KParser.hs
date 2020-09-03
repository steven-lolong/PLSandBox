module KParser (module KParser) where

import Control.Applicative
import Data.Char

newtype Parser a = Parser (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (Parser p) = p 

item :: Parser Char 
item = Parser (\inp -> case inp of
        []      -> []
        (x:xs)  -> [(x,xs)])

instance Functor Parser where
        -- fmap :: (a -> b) -> Parser a -> Parser b
        fmap g p = Parser (\inp -> case parse p inp of
                []              -> []
                [(v,out)]       -> [(g v, out)])


instance Applicative Parser where 
        -- pure :: a -> Parser a
        pure a = Parser (\inp -> [(a,inp)])

        -- <*> :: Parser (a -> b) -> Parser a -> Parser b
        pfab <*> pa = Parser(\inp -> case parse pfab inp of
                []      -> []
                [(f,rem)] -> parse (fmap f pa) rem)


instance Monad Parser where 
        -- >>= :: Parser a -> (a -> Parser b) -> Parser b
        pa >>= fa = Parser (\inp -> case parse pa inp of
                []              -> []
                [(a,out)]       -> parse (fa a) out)

instance Alternative Parser where 
        -- empty :: Parser a 
        empty = Parser (\inp -> [])

        -- (<|>) :: Parser a -> Parser a -> Parser a 
        p <|> q = Parser (\inp -> case parse p inp of
                []              -> parse q inp
                [(x,out)]       -> [(x,out)])


sat :: (Char -> Bool) -> Parser Char
sat f = do
        x <- item
        if f x then return x else empty

-- Numerical
digit :: Parser Char 
digit = sat isDigit 

lower :: Parser Char 
lower = sat isLower

upper :: Parser Char
upper = sat isUpper 

letter :: Parser Char 
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char 
char x = sat (== x)

string :: String -> Parser String 
string []       = return []
string (x:xs)   = do 
        char x 
        string xs 
        return (x:xs)

ident :: Parser String 
ident   = do
        x <- lower 
        xs <- many alphanum
        return (x:xs)

nat :: Parser Int 
nat = do
        xs <- some digit 
        return (read xs)

int :: Parser Int 
int = do 
        char '-'
        n <- nat 
        return (-n)
        <|> nat 

-- Handling spacing
space :: Parser () 
space = do 
        many (sat isSpace)
        return ()

token :: Parser a -> Parser a 
token p = do 
        space 
        v <- p
        space 
        return v

identifier :: Parser String 
identifier = token ident 

symbol :: String -> Parser String
symbol xs = token (string xs)