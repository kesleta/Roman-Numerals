{-# LANGUAGE LambdaCase #-}
module Parser (Parser(..), runSections) where
import           Control.Applicative            ( Alternative
                                                  ( (<|>)
                                                  , empty
                                                  , many
                                                  )
                                                )
import           Control.Monad                  ( MonadPlus(..)
                                                , void
                                                )
import           Data.List                      ( group )
import           Validate

newtype Parser c a = Parser { runParser :: [c] -> Validate [String] (a, [c])}

instance Functor (Parser c) where
  fmap f (Parser p) = Parser $ \s -> do
    (x, s') <- p s
    return (f x, s')

instance Applicative (Parser c) where
  pure a = Parser $ \s -> Success (a, s)
  (Parser f) <*> (Parser x) = Parser $ \s -> do
    (f', s' ) <- f s
    (x', s'') <- x s'
    return (f' x', s'')

instance Monad (Parser c) where
  return = pure
  (Parser p) >>= f = Parser $ \s -> do
    (x, s') <- p s
    runParser (f x) s'

instance Alternative (Parser c) where
  empty = Parser $ const empty
  (Parser a) <|> (Parser b) = Parser $ \s -> a s <|> b s

instance MonadPlus (Parser c) where
  mzero = empty
  mplus = (<|>)

first :: Parser c c
first = Parser $ \case
  (x : xs) -> Success (x, x : xs)
  []       -> Failure ["Empty List"]

charP :: Eq c => c -> Parser c c
charP c = Parser $ \case
  (x : xs) | x == c    -> Success (c, xs)
           | otherwise -> Failure ["Charecter does not match"]
  [] -> Failure ["Empty List"]

complete :: Parser c a -> Parser c a
complete (Parser p) = Parser $ \s -> do
  (x, s') <- p s
  if null s' then Success (x, s') else Failure ["Did not fully consume input"]

noQuad :: Parser Int ()
noQuad = Parser $ \case
  [] -> Failure ["Empty List"]
  s  -> if (< 4) $ maximum $ length <$> group s then Success ((), s) else Failure ["More than 3 consecutive elements"]

i :: Int -> Parser Int ()
i n = void $ many (charP n)

iv :: Int -> Parser Int ()
iv n = void $ charP n *> charP (n * 5)

vi :: Int -> Parser Int ()
vi n = void $ charP (n * 5) *> many (charP n)

ix :: Int -> Parser Int ()
ix n = void $ charP n *> charP (n * 10)

section :: Int -> Parser Int ()
section n = ix n <|> iv n <|> vi n <|> i n

sections :: Parser Int ()
sections = complete $ noQuad *> firstTen >>= sections'
 where
  sections' :: Int -> Parser Int ()
  sections' 1 = section 1
  sections' n = section n *> sections' (n `div` 10)

firstTen :: Parser Int Int
firstTen = Parser $ \case
  (x : xs) -> if f x then Success (x, x : xs) else Success (div x 5, x : xs)
  []       -> Failure ["Empty List"]
 where
  f :: Int -> Bool
  f n | n <= 0    = False
      | otherwise = (/= 5) $ read $ return $ head $ show n

runSections :: [Int] -> Validate [String] ()
runSections = void . runParser sections
