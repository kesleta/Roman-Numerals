module Validate where
import Data.Monoid
import Control.Applicative
import Control.Monad

data Validate err a = Failure err | Success a deriving (Show, Eq)

instance Functor (Validate err) where
  fmap f (Failure err) = Failure err
  fmap f (Success a  ) = Success (f a)

instance Monoid err => Applicative (Validate err) where
  pure = Success
  (Failure a) <*> (Failure b) = Failure (a <> b)
  (Failure a) <*> _           = Failure a
  _           <*> (Failure a) = Failure a
  (Success f) <*> (Success a) = Success (f a)

instance Monoid err => Monad (Validate err) where
  return = Success
  (Success a  ) >>= f = f a
  (Failure err) >>= _ = Failure err

instance Monoid err => Alternative (Validate err) where
  empty = Failure mempty
  (Failure a) <|> (Failure b) = Failure (a <> b)
  (Success a) <|>  _          = Success a
  (Failure _) <|> (Success a) = Success a

instance Monoid err => MonadPlus (Validate err) where
  mzero = empty
  mplus = (<|>)

toMaybe :: Validate err a -> Maybe a
toMaybe (Failure _) = Nothing
toMaybe (Success a) = Just a

fromMaybe :: err -> Maybe a -> Validate err a
fromMaybe err Nothing  = Failure err
fromMaybe err (Just a) = Success a

toEither :: Validate err a -> Either err a
toEither (Failure err) = Left err
toEither (Success a  ) = Right a

fromEither :: Either err a -> Validate err a
fromEither (Left  err) = Failure err
fromEither (Right a  ) = Success a

fromPred :: err -> (a -> Bool) -> a -> Validate err a
fromPred err p a | p a       = Success a
                 | otherwise = Failure err

validate :: (a -> c) -> (e -> c) -> Validate e a -> c
validate f _ (Success a) = f a
validate _ f (Failure e) = f e