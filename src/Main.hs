{-# LANGUAGE TypeApplications, OverloadedStrings, ViewPatterns, FlexibleInstances #-}
module Main where

import Control.Applicative
import Data.Char
import Data.Either
import Data.Ratio
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO


data Token = TNum Rational | TOp Text deriving Show

data Input = Input
  { inputLoc :: Int
  , inputStr :: Text
  } deriving (Show, Eq)

inputUncons :: Input -> Maybe (Char, Input)
inputUncons (Input _ t) | T.null t = Nothing
inputUncons (Input loc t )         = T.uncons t >>= \(x, xs) -> Just (x, Input (loc + 1) xs)

data ParserError = ParserError Int Text deriving (Show)

newtype Parser a = Parser
  { runParser :: Input -> Either ParserError (Input, a)
  }

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', x) <- p input
      return (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Right (input, x)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (input', f) <- p1 input
      (input'', a) <- p2 input'
      return (input'', f a)

instance {-# OVERLAPPING #-} Alternative (Either ParserError) where
  empty = Left $ ParserError 0 "empty"
  Left _ <|> e2 = e2
  e1 <|> _ = e1

instance Alternative Parser where
  empty = Parser $ const empty
  (Parser p1) <|> (Parser p2) =
    Parser $ \input -> p1 input <|> p2 input

charP :: Char -> Parser Char
charP x = Parser f
  where
    f input@(inputUncons -> Just (y, ys))
      | y == x = Right (ys, x)
      | otherwise =
        Left $
        ParserError
          (inputLoc input)
          ("Expected '" <> T.singleton x <> "', but found '" <>  T.singleton  y <> "'")
    f input =
      Left $
      ParserError
        (inputLoc input)
        ("Expected '" <> T.singleton x <> "', but reached end of string")

ws :: Parser Text
ws = spanP "whitespace character" isSpace

spanP :: Text -> (Char -> Bool) -> Parser Text
spanP desc = (\f g -> T.pack <$> f g) $ many . parseIf desc

parseIf :: Text -> (Char -> Bool) -> Parser Char
parseIf desc f =
  Parser $ \input ->
    case input of
      (inputUncons -> Just (y, ys))
        | f y -> Right (ys, y)
        | otherwise ->
          Left $
          ParserError
            (inputLoc input)
            ("Expected " <> desc <> ", but found '" <> T.singleton y <> "'")
      _ ->
        Left $
        ParserError
          (inputLoc input)
          ("Expected " <> desc <> ", but reached end of string")

doubleLiteral :: Parser Double
doubleLiteral =
  (\sign int frac expo ->
       sign * (int + frac) * (10 ** expo))
    <$> (minus <|> pure 1)
    <*> (read <$> digits)
    <*> opt (read <$> (('0':) <$> ((:) <$> charP '.' <*> digits)))
    <*> opt (e *> ((*) <$> (plus <|> minus <|> pure 1) <*> (read <$> digits)))
  where
    digits = some $ parseIf "digit" isDigit
    minus = (-1) <$ charP '-'
    plus = 1 <$ charP '+'
    e = charP 'e' <|> charP 'E'
    opt = (<|> pure 0)

wsBracket :: Parser a -> Parser a
wsBracket p = ws *> p <* ws

numba :: Parser Token
numba = TNum . toRational <$> (wsBracket doubleLiteral)

opSymbols :: String
opSymbols = "+-/*%^$!~&|=><"

opsym :: Parser Char
opsym = parseIf "operator" (`elem` opSymbols)

operata :: Parser Token
operata = TOp . T.pack <$> wsBracket (some opsym)

tokah :: Parser Token
tokah = numba<|> operata

parse :: Text -> Either Text [Token]
parse = go [] . Input 0
    where
      go acc (Input _ x) | T.null x = Right $ reverse acc
      go acc input = case runParser tokah input of
        Left (ParserError n s) -> Left ("Error: " <> s <> " at " <> (T.pack . show $ n))
        Right (input1, tok) -> go (tok:acc) input1

evalOp op x y = case op of
    "+" -> x + y
    "-" -> x - y
    "*" -> x * y
    "/" -> x / y
    "%" -> fromInteger $ mod (floor x) (floor y)
    _ -> x

eval :: Rational -> [Token] -> Either Text (Rational, Rational)
eval mem s = go s []
    where go [TNum x] _ = return (x, x)
          go s@(TNum x:_) _ | all isTNum s = return (x, x)
          go [TNum x, TOp op] [] = go [TNum mem, TNum x, TOp op] []
          go (TNum x: TNum y: TOp op:xs) ys  =  go (TNum (evalOp op x y):xs) ys
          go (TNum x: TOp op:xs) (TNum y:ys)  = go (TNum y: TNum x: TOp op:xs) ys
          go (TNum x: TNum y: TNum z:xs) ys  =  go (TNum y: TNum z:xs) (TNum x:ys)
          go _ _ = Left "Error: doing it wrong!"
          isTNum (TNum _) = True
          isTNum _ = False

showRational :: Rational -> Text
showRational r = if denominator r == 1 
    then showT $ numerator r 
    else showT @Double $ fromRational r

showT :: Show a => a -> Text
showT = T.pack . show

main :: IO ()
main = go 0

go :: Rational -> IO()
go mem = do
    TIO.putStr "> "
    hFlush stdout
    x <- TIO.getLine
    case parse x >>= eval mem of
        Left s -> do
            TIO.putStrLn s
            go mem
        Right (res, mem0) -> do
            TIO.putStrLn . showRational $ res
            go mem0
