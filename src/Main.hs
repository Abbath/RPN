{-# LANGUAGE TypeApplications, OverloadedStrings, ViewPatterns, FlexibleInstances, LambdaCase #-}
module Main where

import Control.Applicative
import Data.Char
import Data.Either
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ratio
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO


data Token = TNum Rational | TOp Text | TIdent Text deriving Show

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
opSymbols = "+-/*%^="

opsym :: Parser Char
opsym = parseIf "operator" (`elem` opSymbols)

operata :: Parser Token
operata = TOp . T.pack <$> wsBracket (some opsym)

alfa :: Parser Char
alfa = parseIf "letters and _" ((||) <$> isAlpha <*> (== '_'))

alfaNum :: Parser Char
alfaNum = alfa <|> parseIf "alfas and numbas" isDigit

ident :: Parser Token
ident = TIdent <$> wsBracket (T.cons <$> alfa <*> (T.pack <$> many alfaNum))

tokah :: Parser Token
tokah = numba<|> operata <|> ident

parse :: Text -> Either Text [Token]
parse = go [] . Input 0
    where
      go acc (Input _ x) | T.null x = Right $ reverse acc
      go acc input = case runParser tokah input of
        Left (ParserError n s) -> Left ("Error: " <> s <> " at " <> (T.pack . show $ n))
        Right (input1, tok) -> go (tok:acc) input1

pow :: Rational -> Rational -> Rational
pow a b | d2 a b && numerator b < 0 = toRational $ (fromRational a :: Double) ^^ numerator b
        | d2 a b = toRational $ numerator a ^ numerator b
        | otherwise = toRational $ (fromRational a :: Double) ** (fromRational b :: Double)
  where
    d1 = (==1) . denominator
    d2 a b = d1 a && d1 b

evalOp :: Text -> Rational -> Rational -> Rational
evalOp op x y = case op of
    "+" -> x + y
    "-" -> x - y
    "*" -> x * y
    "/" -> x / y
    "^" -> pow x y
    "%" -> fromInteger $ mod (floor x) (floor y)
    _ -> x

funs :: [Text]
funs = ["sin", "cos", "tan", "asin", "acos", "atan", "log", "exp", "sqrt"]

evalFun :: Text -> Rational -> Rational
evalFun fun x = toRational $ (case fun of
    "sin" -> sin . fr
    "cos" -> cos . fr
    "tan" -> tan . fr
    "asin" -> asin . fr
    "acos" -> acos . fr 
    "atan" -> \b -> atan2 (fromInteger (numerator b)) (fromInteger (denominator b)) 
    "log" -> log . fr
    "sqrt" -> sqrt . fr
    "exp" -> exp . fr ) x
  where
    fr a = fromRational a :: Double 

type Vars = Map Text Rational

eval :: Vars -> [Token] -> Either Text (Rational, Vars)
eval vars s = go (substitute s) []
    where go [TNum x] _ = return (x, M.insert ("_" :: Text) x vars)
          go [TIdent x, TNum y, TOp "="] []  = return (y, M.insert x y vars)
          go s@(TNum x:_) _ | all isTNum s = return (x, M.insert "_" x vars)
          go [TNum x, TOp op] [] = go [TNum (vars M.! "_"), TNum x, TOp op] []
          go (TNum x: TIdent i:xs) ys | i `elem` funs = go (TNum (evalFun i x):xs) ys
          go (TNum x: TNum y: TOp op:xs) ys  =  go (TNum (evalOp op x y):xs) ys
          go (TNum x: TOp op:xs) (y:ys)  = go (y: TNum x: TOp op:xs) ys
          go (x: TNum y: TNum z:xs) ys  =  go (TNum y: TNum z:xs) (x:ys)
          go (x: TNum y: TIdent i:xs) ys | i `elem` funs  =  go (TNum y: TIdent i:xs) (x:ys)
          go _ _ = Left "Error: doing it wrong!"
          isTNum (TNum _) = True
          isTNum _ = False
          substitute s@[TIdent _, TNum _, TOp "="] = s
          substitute s = map (\case
              b@(TIdent x) -> if M.member x vars then TNum (vars M.! x) else b
              a -> a) s

showRational :: Rational -> Text
showRational r = if denominator r == 1
    then showT $ numerator r
    else showT @Double $ fromRational r

showT :: Show a => a -> Text
showT = T.pack . show

main :: IO ()
main = go (M.singleton "_" 0)

go :: Vars -> IO()
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
