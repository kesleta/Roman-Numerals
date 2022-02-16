{-# LANGUAGE LambdaCase #-}
module Parser where
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

newtype Parser c a = Parser { runParser :: [c] -> Maybe (a, [c])}

instance Functor (Parser c) where
  fmap f (Parser p) = Parser $ \s -> do
    (x, s') <- p s
    return (f x, s')

instance Applicative (Parser c) where
  pure a = Parser $ \s -> Just (a, s)
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
  empty = Parser $ const Nothing
  (Parser a) <|> (Parser b) = Parser $ \s -> a s <|> b s

instance MonadPlus (Parser c) where
  mzero = empty
  mplus = (<|>)

first :: Parser c c
first = Parser $ \case
  (x : xs) -> Just (x, x : xs)
  []       -> Nothing

charP :: Eq c => c -> Parser c c
charP c = Parser $ \case
  (x : xs) | x == c    -> Just (c, xs)
           | otherwise -> Nothing
  [] -> Nothing

complete :: Parser c a -> Parser c a
complete (Parser p) = Parser $ \s -> do
  (x, s') <- p s
  if null s' then Just (x, s') else Nothing

noQuad :: Parser Int ()
noQuad = Parser $ \case
  [] -> Nothing
  s  -> if (< 4) $ maximum $ length <$> group s then Just ((), s) else Nothing

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
  (x : xs) -> if f x then Just (x, x : xs) else Just (div x 5, x : xs)
  []       -> Nothing
 where
  f :: Int -> Bool
  f n | n <= 0    = False
      | otherwise = (/= 5) $ read $ return $ head $ show n

runSections :: [Int] -> Maybe ()
runSections = void . runParser sections
