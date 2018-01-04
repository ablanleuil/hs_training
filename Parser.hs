{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, UndecidableInstances #-}

module Parser where

import Control.Monad.State
import Control.Monad.Except

data Arith = Plus Arith Arith | Mult Arith Arith | Atom Int deriving (Show)
data ParseError = ParseError String deriving (Show)

type Parser tok r = StateT [tok] (Either ParseError) r

throw :: String -> Parser tok r
throw = throwError . ParseError

-- same as regexp (a|b)
(<|>) a b = a `catchError` const b
(<?>) p msg = p <|> throw msg

-- if [p] fails, [try p] doesn't consume the stream
try :: Parser tok r -> Parser tok r
try p = do
  pre <- get

  p `catchError` (\e -> put pre >> throwError e)

-- parse a single symbol from the stream
sym :: (Eq tok) => tok -> Parser tok tok
sym c = do
  l <- get
  case l of
    (x:xs) | x == c -> put xs >> return c
    _               -> throw $ "Wrong symbol"

-- if [p] fails, [option r p] returns [r], BUT consumes the stream
-- see [try] for this
option :: r -> Parser tok r -> Parser tok r
option r p = p <|> return r

-- same as regexp (+)
many1 :: Parser tok r -> Parser tok [r]
many1 = liftM2 (:) <$> id <*> many

-- same as regexp (*)
many :: Parser tok r -> Parser tok [r]
many = option [] . many1

oneOf :: (Eq tok) => [tok] -> Parser tok tok
oneOf = foldr ((<|>) . sym) (throw "oneOf failed") 

digit = oneOf "0123456789"

integer :: Parser Char Int
integer = fmap read $ many1 digit

arith :: Parser Char Arith
arith = plus

plus = do
  gauche <- mult
  plus' gauche

plus' gauche = option gauche $ do
  sym '+'
  droite <- mult
  plus' (Plus gauche droite)

mult = do
  gauche <- atom
  mult' gauche

mult' gauche = option gauche $ do
  sym '*'
  droite <- atom
  mult' (Mult gauche droite)

atom = fmap Atom integer

